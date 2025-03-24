#' Generate Samples from Nested Sampling
#'
#' Use this function to perform nested sampling until a given criterion is met.
#' Calling [generate()] implicitly calls [compile()] to ensure the sampler
#' object is valid before running samples.
#'
#' @param x An object of class `ernest_sampler`.
#' @param max_iterations The maximum number of iterations to perform. If set to
#' Inf, this stopping criterion is ignored.
#' @param max_calls The maximum number of calls to the likelihood function.
#' If set to Inf, this stopping criterion is ignored.
#' @param min_logz The minimum log-evidence value to achieve. Must be a number
#' strictly larger than zero.
#' @param refresh Whether to clear existing points from the sampler, starting
#' a run from scratch.
#' @param ... Must be empty.
#'
#' @returns `x`, invisibly.
#' @export
generate.ernest_sampler <- function(x,
                                    max_iterations = Inf,
                                    max_calls = Inf,
                                    min_logz = 0.05,
                                    refresh = FALSE,
                                    ...) {
  check_dots_empty()
  x$generate(
    max_iterations = max_iterations,
    max_calls = max_calls,
    min_logz = min_logz,
    refresh = refresh
  )
}
