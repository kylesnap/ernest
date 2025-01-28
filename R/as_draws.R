#' @importFrom posterior as_draws_matrix as_draws_rvars
#' @export
posterior::as_draws_matrix
#' @export
posterior::as_draws_rvars

#' Transform to draws objects
#'
#' @param x The earnest run object
#' @param resample_draws Whether to run `posterior::resample_draws` on the
#' draws object, producing a sample from a resampled posterior.
#' @param ... Additional arguments to pass to `posterior::resample_draws`
#'
#' @details When `resample_draws` is false, the draws object will be bound to
#' the log importance weights from the nested sampling run.
#'
#' @returns An object of class `"draws_matrix"`.
#'
#' @export
as_draws_matrix.ernest_run <- function(x, resample_draws = TRUE, ...) {
  draws <- posterior::as_draws_matrix(x$samples[, -1])
  if (resample_draws) {
    draws <- posterior::resample_draws(draws, ...)
  }
  draws
}

#' Transform to draws objects
#'
#' @param x The earnest run object
#' @param resample_draws Whether to run `posterior::resample_draws` on the
#' draws object, producing a sample from a resampled posterior.
#' @param ... Additional arguments to pass to `posterior::resample_draws`
#'
#' @details When `resample_draws` is false, the draws object will be bound to
#' the log importance weights from the nested sampling run.
#'
#' @returns An object of class `"draws_matrix"`.
#'
#' @export
as_draws_rvars.ernest_run <- function(x, resample_draws = TRUE, ...) {
  draws <- posterior::as_draws_rvars(x$samples[, -1])
  if (resample_draws) {
    draws <- posterior::resample_draws(draws, ...)
  }
  draws
}
