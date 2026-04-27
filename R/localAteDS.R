#' Compute local average treatment effect from regression coefficients
#'
#' Server-side function that computes the local average treatment effect (ATE)
#' using regression coefficients provided by the client after federated linear
#' regression. The function calculates the local ATE for the data on this server
#' and returns it along with the sample size to the client for aggregation across
#' all servers. This is an aggregate function.
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
#' @returns A list containing:
#'   \item{ate}{The local average treatment effect estimate \eqn{\tau_k}}
#'   \item{n}{The sample size \eqn{n_k} on this server}
#' @export
localAteDS <- function(base_name, covariate_names_str, treatment_name, tau_str, gamma_str) {
  # Parse arguments intro appropriate types ----------------------------------------

  covariate_names <- scan(text = covariate_names_str, what = "", sep = ",", strip.white = TRUE, quiet = TRUE)
  tau <- suppressWarnings(as.numeric(tau_str))
  gamma <- suppressWarnings(
    as.numeric(unlist(strsplit(gamma_str, split = ",", fixed = TRUE)))
  )

  base <- eval(parse(text = base_name), envir = parent.frame())
  n_k <- nrow(base)

  # Computation of local Ate -------------------------------------------------------

  X_mat <- as.matrix(base[, covariate_names, drop = FALSE])
  x_bar_k <- colMeans(X_mat, na.rm = TRUE)
  tau_k <- as.numeric(tau + sum(x_bar_k * gamma))

  return(list(
    n_k = n_k,
    tau_k = tau_k
  ))
}
