#' Compute local statistics for the federated ATE and (optionally) its variance
#'
#' Server-side aggregate function called after the federated GLM has converged.
#' Always returns the quantities needed for the ATE point estimate.  When
#' \code{compute_variance = TRUE} (the default) it also returns the residual
#' sum of squares and the projected scatter scalar required for the delta-method
#' variance; set it to \code{FALSE} to limit disclosure to the minimum needed
#' for the point estimate only.
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
#'   return \code{rss_k} and \code{s_k_scalar} for variance estimation.
#' @param heterogeneous Logical. If \code{TRUE} (default) and
#'   \code{compute_variance = TRUE}, also return \code{n_k_X_bar} (column sums
#'   of the covariate design matrix, equal to n_k times the local covariate
#'   mean), needed for the leverage-corrected variance estimator under full
#'   heterogeneity (different covariate distributions and treatment probabilities
#'   across sites).  Set to \code{FALSE} to suppress this extra disclosure when
#'   heterogeneity is known to be absent.  Has no effect when
#'   \code{compute_variance = FALSE}.
#'
#' @return A list with three elements always present:
#'   \item{n_k}{Total sample size on this server.}
#'   \item{n_k_1}{Number of treated observations on this server.}
#'   \item{tau_k}{Local ATE estimate \eqn{\hat\tau_k}.}
#'   When \code{compute_variance = TRUE}, two additional elements are included:
#'   \item{s_k_scalar}{Local sum of squared deviations of the projected index
#'     \eqn{X_i^\top\Delta\hat\beta} from its local mean; used to pool
#'     \eqn{\Delta\hat\beta^\top\hat\Sigma\Delta\hat\beta}.}
#'   \item{rss_k}{Residual sum of squares across both arms using the federated
#'     coefficients; used to compute \eqn{\hat\sigma^2}.}
#'   When \code{compute_variance = TRUE} and \code{heterogeneous = TRUE}, one
#'   further element is included:
#'   \item{n_k_X_bar}{Numeric vector of length \eqn{d} containing the column
#'     sums of the covariate matrix (\eqn{n_k \bar{X}_k}); used by the client
#'     to assemble the global covariate mean for the leverage-corrected
#'     OLS noise component of the variance.}
#' @references
#' Khellaf, R., Bellet, A., and Josse, J. (2025).
#' Federated Causal Inference: Multi-Study ATE Estimation beyond Meta-Analysis.
#' \emph{Proceedings of the 28th International Conference on Artificial
#' Intelligence and Statistics (AISTATS 2025)}.
#' @importFrom dsBase checkPermissivePrivacyControlLevel listDisclosureSettingsDS
#' @importFrom cli cli_abort
#' @importFrom stats model.matrix as.formula
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
                       compute_variance = TRUE, heterogeneous = TRUE) {

  # 1. Privacy control level check
  dsBase::checkPermissivePrivacyControlLevel(c("permissive", "banana", "carrot"))

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

  delta_alpha <- alpha1 - alpha0
  gamma       <- beta1  - beta0

  n_k   <- nrow(base)
  idx_1 <- W == 1L

  # 5. Local ATE and projected scatter
  #    proj_bar_k = X̄_k · γ̂ = mean(X_i · γ̂); no need to expose X̄_k itself.
  proj_i     <- as.numeric(X_mat %*% gamma)
  proj_bar_k <- mean(proj_i)
  tau_k      <- as.numeric(delta_alpha + proj_bar_k)

  # 6. Number of treated observations
  n_k_1 <- sum(idx_1)

  if (!compute_variance) {
    return(list(n_k = n_k, n_k_1 = n_k_1, tau_k = tau_k))
  }

  # 7. Residual sum of squares across both arms
  pred_1 <- alpha1 + as.numeric(X_mat[ idx_1, , drop = FALSE] %*% beta1)
  pred_0 <- alpha0 + as.numeric(X_mat[!idx_1, , drop = FALSE] %*% beta0)
  rss_k  <- sum((Y[ idx_1] - pred_1)^2) + sum((Y[!idx_1] - pred_0)^2)

  # 8. Within-site projected scatter (parallel-axis decomposition)
  s_k_scalar <- sum((proj_i - proj_bar_k)^2)

  result <- list(
    n_k        = n_k,
    n_k_1      = n_k_1,
    tau_k      = tau_k,
    s_k_scalar = s_k_scalar,
    rss_k      = rss_k
  )
  if (heterogeneous) result$n_k_X_bar <- colSums(X_mat)
  return(result)
}

#' @keywords internal
#' @noRd
.checkDisclosureRisk <- function(data) {
  thr            <- dsBase::listDisclosureSettingsDS()
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  if (nrow(data) < nfilter.subset) {
    cli::cli_abort(
      c(
        "x" = "Disclosure risk: dataset has too few rows to proceed.",
        "i" = "Minimum required: {nfilter.subset}, provided: {nrow(data)}"
      )
    )
  }
}

#' @keywords internal
#' @noRd
.parse_numeric_scalar <- function(x_str, arg_name) {
  val <- suppressWarnings(as.numeric(x_str))
  if (is.na(val)) {
    cli::cli_abort(c(
      "x" = "`{arg_name}` could not be parsed as a numeric value.",
      "i" = "Received: {x_str}"
    ))
  }
  val
}

#' @keywords internal
#' @noRd
.parse_numeric_vector <- function(x_str, arg_name) {
  vals <- suppressWarnings(
    as.numeric(unlist(strsplit(x_str, split = ",", fixed = TRUE)))
  )
  if (any(is.na(vals))) {
    cli::cli_abort(c(
      "x" = "`{arg_name}` could not be fully parsed as a numeric vector.",
      "i" = "Received: {x_str}"
    ))
  }
  vals
}
