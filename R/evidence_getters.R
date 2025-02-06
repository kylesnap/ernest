#' Get a `tibble` of a nested sampling run
#'
#' @param x An object of class `ernest_run`
#' @param exponentiate Logical indicating whether or not to exponentiate the
#' likelihood, importance weight, and evidence values.
#' @param normalise Logical indicating whether or not to normalise the
#' likelihood values.
#'
#' @returns A `tibble` with columns
#' @export
summarise_run_metrics <- function(x, exponentiate = TRUE, normalise = FALSE) {
  if (!inherits(x, "ernest_run")) {
    cli::cli_abort("Input must be an `ernest_run` object")
  }

  log_vol <- x$integration$log_vol
  log_lik <- if (normalise) {
    x$integration$log_lik - max(x$integration$log_lik)
  } else {
    x$integration$log_lik
  }
  log_z <- x$integration$log_z
  log_z_var <- x$integration$log_z_var
  log_z_var[!is.finite(log_z_var)] <- 0
  log_importance <- x$samples$.log_weight

  out <- tibble::tibble(
    "log_volume" = log_vol,
    "likelihood" = if (exponentiate) exp(log_lik) else log_lik,
    "evidence" = if (exponentiate) exp(log_z) else log_z,
    "evidence.var" = if (exponentiate) exp(log_z_var) else log_z_var,
    "importance" = if (exponentiate) exp(log_importance) else log_importance
  )
  if (!exponentiate) {
    names(out)[2:5] <- paste0("log_", names(out)[2:5])
  }
  if (normalise) {
    names(out)[2] <- paste0(names(out)[2], ".norm")
  }
  out
}
