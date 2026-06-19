#' Compute local statistics for the federated ATE and (optionally) its variance
#'
#' Server-side aggregate function called after the federated GLM has converged.
#' Always returns the quantities needed for the ATE point estimate.  When
#' \code{compute_variance = TRUE} (the default) it also returns the statistics
#' required for the delta-method variance; the exact set depends on
#' \code{outcome_type} (see \strong{Value}).  Set \code{compute_variance = FALSE}
#' to limit disclosure to the minimum needed for the point estimate only.
#'
#' @param base_name A character string naming the server-side data frame.
#' @param outcome_name A character string naming the outcome variable inside
#'   \code{base_name}.
#' @param covariate_names_str A character string of comma-separated covariate
#'   names.
#' @param treatment_name A character string naming the binary treatment
#'   indicator inside \code{base_name}.
#' @param alpha0_str Character. Intercept of the control-arm model.
#' @param beta0_str Character. Comma-separated slope coefficients of the
#'   control-arm model.
#' @param alpha1_str Character. Intercept of the treated-arm model.
#' @param beta1_str Character. Comma-separated slope coefficients of the
#'   treated-arm model.
#' @param compute_variance Logical. If \code{TRUE} (default), also compute and
#'   return the quantities needed for variance estimation.  For
#'   \code{outcome_type = "continuous"} these are \code{rss_k} and
#'   \code{s_k_scalar}; for \code{outcome_type = "binary"} they are the local
#'   Fisher information matrices, gradient vectors, and delta scatter scalar
#'   (see \strong{Value}).  Set to \code{FALSE} to limit disclosure to the
#'   three scalars strictly needed for the point estimate.
#' @param heterogeneous Logical. If \code{TRUE} (default) and
#'   \code{compute_variance = TRUE} and \code{outcome_type = "continuous"},
#'   also return \code{n_k_X_bar} (column sums of the covariate design matrix,
#'   equal to \eqn{n_k \bar{X}_k}), needed for the leverage-corrected variance
#'   estimator.  Set to \code{FALSE} to suppress this extra disclosure when
#'   heterogeneity is known to be absent.  Has no effect when
#'   \code{compute_variance = FALSE} or \code{outcome_type = "binary"}.
#' @param outcome_type A character string, either \code{"continuous"} (default)
#'   or \code{"binary"}. When \code{"binary"}, the local ATE is computed via
#'   G-computation: \eqn{\hat\tau_k = n_k^{-1}\sum_i[\mu_1(X_i) - \mu_0(X_i)]}
#'   where \eqn{\mu_w(x) = \sigma(\hat\alpha_w + x^\top\hat\beta_w)}.
#'   When \code{compute_variance = TRUE}, the return list additionally contains
#'   the gradient raw sums and delta scatter; see \strong{Value}.  The global
#'   Fisher information matrix \eqn{\mathbf{H}_w^{-1}} is not returned here —
#'   the client obtains it directly from
#'   \code{ds.glm(viewVarCov = TRUE)\$VarCovMatrix}, which equals
#'   \eqn{\mathbf{H}_w^{-1}} for binomial models (dispersion = 1).
#'
#' @return A list with at least three elements:
#'   \item{n_k}{Total sample size on this server.}
#'   \item{n_k_1}{Number of treated observations on this server.}
#'   \item{tau_k}{Local ATE estimate \eqn{\hat\tau_k}.}
#'   When \code{compute_variance = TRUE}, three additional elements common to
#'   both outcome types:
#'   \item{s_k}{Local scatter \eqn{\sum_{i \in k}(\delta_i - \hat\tau_k)^2}.
#'     For continuous outcomes \eqn{\delta_i = X_i^\top\Delta\hat\beta}; for
#'     binary outcomes \eqn{\delta_i = \hat\mu_1(x_i) - \hat\mu_0(x_i)}.}
#'   \item{g1_raw_k}{Unnormalised gradient sum for the treated arm; the client
#'     divides by global \eqn{n}.  For continuous outcomes this equals
#'     \eqn{(n_k,\, \sum_i X_{ij})}, i.e.\ column sums of \eqn{\tilde{X}};
#'     for binary outcomes \eqn{\sum_{i \in k} v_1(x_i)\tilde{x}_i}.}
#'   \item{g0_raw_k}{Analogous sum for the control arm (equals \code{g1_raw_k}
#'     for continuous outcomes).}
#'   For \code{outcome_type = "continuous"} with \code{compute_variance = TRUE},
#'   one further element:
#'   \item{rss_k}{Residual sum of squares across both arms using the federated
#'     coefficients; used to compute \eqn{\hat\sigma^2}.}
#'   When \code{outcome_type = "continuous"} and \code{heterogeneous = FALSE},
#'   \code{g1_raw_k} and \code{g0_raw_k} are omitted.
#' @references
#' Khellaf, R., Bellet, A., and Josse, J. (2025).
#' Federated Causal Inference: Multi-Study ATE Estimation beyond Meta-Analysis.
#' \emph{Proceedings of the 28th International Conference on Artificial
#' Intelligence and Statistics (AISTATS 2025)}.
#' @importFrom dsBase checkPermissivePrivacyControlLevel
#' @importFrom cli cli_abort
#' @importFrom stats model.matrix as.formula plogis
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   X1 = rnorm(30), X2 = rnorm(30),
#'   W  = rbinom(30, 1, 0.5)
#' )
#' df$Y <- 1 + 0.5 * df$W + 0.3 * df$X1 + rnorm(30, sd = 0.5)
#' options(datashield.privacyControlLevel = "banana", nfilter.subset = 3)
#' localAteDS("df", "Y", "X1,X2", "W", "1.0", "0.3,0.0", "1.5", "0.3,0.0")
#' }
#' @export
localAteDS <- function(base_name, outcome_name, covariate_names_str, treatment_name,
                       alpha0_str, beta0_str, alpha1_str, beta1_str,
                       compute_variance = TRUE, heterogeneous = TRUE,
                       outcome_type = "continuous") {

  # 1. Privacy control level check
  dsBase::checkPermissivePrivacyControlLevel(c("permissive", "banana", "carrot"))
  outcome_type <- match.arg(outcome_type, c("continuous", "binary"))

  # 2. Parse arguments
  covariate_names <- scan(
    text = covariate_names_str, what = "", sep = ",",
    strip.white = TRUE, quiet = TRUE
  )
  alpha0 <- .parse_numeric_scalar(alpha0_str, "alpha0_str")
  alpha1 <- .parse_numeric_scalar(alpha1_str, "alpha1_str")
  beta0  <- .parse_numeric_vector(beta0_str,  "beta0_str")
  beta1  <- .parse_numeric_vector(beta1_str,  "beta1_str")

  base <- eval(parse(text = base_name), envir = parent.frame())

  # 3. Disclosure check
  .checkDisclosureRisk(base)

  # 4. Extract data components
  W     <- base[[treatment_name]]
  Y     <- base[[outcome_name]]
  X_mat <- model.matrix(
    as.formula(paste("~", paste(covariate_names, collapse = "+"))),
    data = base[, covariate_names, drop = FALSE]
  )[, -1L, drop = FALSE]

  n_k   <- nrow(base)
  idx_1 <- W == 1L
  n_k_1 <- sum(idx_1)

  # 5. ATE point estimate (outcome-specific)
  if (outcome_type == "binary") {
    X_tilde <- cbind(1, X_mat)
    mu1     <- plogis(alpha1 + as.numeric(X_mat %*% beta1))
    mu0     <- plogis(alpha0 + as.numeric(X_mat %*% beta0))
    delta_i <- mu1 - mu0
    tau_k   <- mean(delta_i)
  } else {
    delta_alpha <- alpha1 - alpha0
    gamma       <- beta1  - beta0
    proj_i      <- as.numeric(X_mat %*% gamma)
    proj_bar_k  <- mean(proj_i)
    tau_k       <- as.numeric(delta_alpha + proj_bar_k)
  }

  # 6. Early exit when only the point estimate is needed
  if (!compute_variance) {
    return(list(n_k = n_k, n_k_1 = n_k_1, tau_k = tau_k))
  }

  # 7. Variance statistics — three independent pieces, then one return

  # 7a. Local scatter s_k
  if (outcome_type == "binary") {
    s_k <- sum((delta_i - tau_k)^2)
  } else {
    s_k <- sum((proj_i - proj_bar_k)^2)
  }

  # 7b. Residual sum of squares (continuous only, needed for sigma2_hat)
  if (outcome_type == "continuous") {
    pred_1 <- alpha1 + as.numeric(X_mat[ idx_1, , drop = FALSE] %*% beta1)
    pred_0 <- alpha0 + as.numeric(X_mat[!idx_1, , drop = FALSE] %*% beta0)
    rss_k  <- sum((Y[ idx_1] - pred_1)^2) + sum((Y[!idx_1] - pred_0)^2)
  }

  # 7c. Gradients (binary always; continuous only when heterogeneous = TRUE)
  if (outcome_type == "binary") {
    v1       <- mu1 * (1 - mu1)
    v0       <- mu0 * (1 - mu0)
    g1_raw_k <- colSums(v1 * X_tilde)
    g0_raw_k <- colSums(v0 * X_tilde)
  } else if (heterogeneous) {
    g_raw_k  <- colSums(cbind(1, X_mat))
    g1_raw_k <- g_raw_k
    g0_raw_k <- g_raw_k
  }

  # 7d. Assemble result
  result <- list(n_k = n_k, n_k_1 = n_k_1, tau_k = tau_k, s_k = s_k)
  if (outcome_type == "continuous")                    result$rss_k    <- rss_k
  if (outcome_type == "binary" || heterogeneous) {
    result$g1_raw_k <- g1_raw_k
    result$g0_raw_k <- g0_raw_k
  }
  return(result)
}

