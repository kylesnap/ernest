#' Get a `tibble` containing the integration results of a nested sampling run
#'
#' @param x An object of class `ernest_run`
#' @param exponentiate Whether or not to exponentiate the likelihood values
#' @param error Whether or not to include the estimate of evidence error in the returned table
#'
#' @returns A `tibble` with columns
#' @export
get_integration_df <- function(x, exponentiate = TRUE, error = TRUE) {
  if (!inherits(x, "ernest_run")) {
    cli::cli_abort("Input must be an `ernest_run` object")
  }

  integration <- x$integration
  weights <- x$samples$.log_weight

  if (exponentiate) {
    tibble::tibble(
      "log_vol" = integration$log_vol,
      "likelihood" = exp(integration$log_lik),
      "norm_likelihood" = exp(integration$log_lik - max(integration$log_lik)),
      "weights" = exp(weights),
      "evidence" = exp(integration$log_z),
      "log_evidence_error" = if (error) sqrt(integration$log_z_var) else NULL
    )
  } else {
    tibble::tibble(
      "log_vol" = integration$log_vol,
      "log_lik" = integration$log_lik,
      "log_norm_lik" = integration$log_lik - max(integration$log_lik),
      "log_weights" = weights,
      "log_evidence" = integration$log_z,
      "log_evidence_error" = if (error) sqrt(integration$log_z_var) else NULL
    )
  }
}
