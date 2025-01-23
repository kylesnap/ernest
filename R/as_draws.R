#' @importFrom posterior as_draws_matrix as_draws_rvars
#' @export
posterior::as_draws_matrix
#' @export
posterior::as_draws_rvars

#' Transform to draws objects
#'
#' @param x The earnest run object
#'
#' @returns A `posterior::draws_matrix` object, which has classes
#' `posterior::draws` and `matrix`
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
#'
#' @returns A `posterior::draws_matrix` object, which has classes
#' `posterior::draws` and `matrix`
#'
#' @export
as_draws_rvars.ernest_run <- function(x, resample_draws = TRUE, ...) {
  draws <- posterior::as_draws_rvars(x$samples[, -1])
  if (resample_draws) {
    draws <- posterior::resample_draws(draws, ...)
  }
  draws
}
