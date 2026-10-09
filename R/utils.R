#' @keywords internal
#' @noRd
#' @importFrom dsBase listDisclosureSettingsDS
#' @importFrom cli cli_abort
.checkDisclosureRisk <- function(data, treatment_name = NULL) {
  thr            <- dsBase::listDisclosureSettingsDS()
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  n_k            <- nrow(data)
  if (n_k < nfilter.subset) {
    cli::cli_abort(
      c(
        "x" = "Disclosure risk: dataset has too few rows to proceed.",
        "i" = "Minimum required: {nfilter.subset}, provided: {n_k}"
      )
    )
  }
  if (is.null(treatment_name)) return(invisible(NULL))
  treatment <- data[[treatment_name]]
  n_k_1     <- sum(treatment == 1L, na.rm = TRUE)
  n_k_0     <- sum(treatment == 0L, na.rm = TRUE)
  if (n_k_1 < nfilter.subset) {
    cli::cli_abort(
      c(
        "x" = "Disclosure risk: too few treated units to proceed.",
        "i" = "Minimum required: {nfilter.subset}, provided: {n_k_1}"
      )
    )
  }
  if (n_k_0 < nfilter.subset) {
    cli::cli_abort(
      c(
        "x" = "Disclosure risk: too few control units to proceed.",
        "i" = "Minimum required: {nfilter.subset}, provided: {n_k_0}"
      )
    )
  }
}

#' @keywords internal
#' @noRd
#' @importFrom cli cli_abort
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
#' @importFrom cli cli_abort
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
