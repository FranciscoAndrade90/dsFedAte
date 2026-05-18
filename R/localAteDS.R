#' Compute local average treatment effect from regression coefficients
#'
#' Server-side function that computes the local average treatment effect (ATE)
#' using regression coefficients provided by the client after federated linear
#' regression. The function calculates the local ATE for the data on this server
#' and returns it along with the sample size to the client for aggregation across
#' all servers. This is an aggregate function.
#'
#' @param base_name A character string specifying the name of the data frame on
#'   the server side.
#' @param covariate_names_str A character string containing comma-separated
#'   covariate variable names.
#' @param treatment_name Character. The name of the treatment assignment variable.
#'   Should be a binary indicator distinguishing treatment and control groups.
#' @param tau_str A character string representation of a numeric value for the
#'   difference between treatment and control group intercepts (treatment minus control).
#' @param gamma_str A character string of comma-separated values representing the
#'   differences between treatment and control group regression coefficients for
#'   covariates (treatment minus control).
#'
#' @return A list containing:
#'   \item{n_k}{The sample size \eqn{n_k} on this server}
#'   \item{tau_k}{The local average treatment effect estimate \eqn{\hat{\tau}_k}}
#' @importFrom dsBase checkPermissivePrivacyControlLevel listDisclosureSettingsDS
#' @importFrom cli cli_abort
#' @examples
#' \dontrun{
#' # This function is called server-side via datashield.aggregate().
#' # Direct invocation for local testing only:
#' df <- data.frame(X1 = rnorm(20), X2 = rnorm(20), W = rbinom(20, 1, 0.5))
#' options(datashield.privacyControlLevel = "banana", nfilter.subset = 3)
#' localAteDS("df", "X1,X2", "W", "0.5", "0.1,0.2")
#' }
#' @export
localAteDS <- function(base_name, covariate_names_str, treatment_name, tau_str, gamma_str) {
  # 1. Privacy control level check
  dsBase::checkPermissivePrivacyControlLevel(c("permissive", "banana", "carrot"))

  # 2. Parse arguments into appropriate types
  covariate_names <- scan(
    text = covariate_names_str, what = "", sep = ",",
    strip.white = TRUE, quiet = TRUE
  )
  tau   <- .parse_numeric_scalar(tau_str, "tau_str")
  gamma <- .parse_numeric_vector(gamma_str, "gamma_str")

  base <- eval(parse(text = base_name), envir = parent.frame())

  # 3. Disclosure check
  .checkDisclosureRisk(base)

  n_k <- nrow(base)

  # 4. Computation of local ATE
  X_mat   <- as.matrix(base[, covariate_names, drop = FALSE])
  x_bar_k <- colMeans(X_mat, na.rm = TRUE)
  tau_k   <- as.numeric(tau + sum(x_bar_k * gamma))

  return(list(
    n_k   = n_k,
    tau_k = tau_k
  ))
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
  vals <- suppressWarnings(as.numeric(unlist(strsplit(x_str, split = ",", fixed = TRUE))))
  if (any(is.na(vals))) {
    cli::cli_abort(c(
      "x" = "`{arg_name}` could not be fully parsed as a numeric vector.",
      "i" = "Received: {x_str}"
    ))
  }
  vals
}
