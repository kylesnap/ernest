#' Prepare an `ernest_sampler` object for nested sampling
#'
#' @param An `ernest_sampler` object.
#' @param clear A logical value indicating whether to reload `object` before
#' creating new live points. If `TRUE`, former results from the sampler are
#' removed from the object. If `FALSE`, the sampler will not drop prior results,
#' and continue using live points from the last sampling iteration performed.
#' @param ... Must be empty.
#'
#' @returns `object`, invisibly.
#' @details This method wraps a call to [ernest_sampler$compile()].
#' @importFrom generics compile
#' @method compile ernest_sampler
#' @export
compile.ernest_sampler <- function(object, clear = FALSE, ...) {
  check_dots_empty()
  object$compile(clear)
}

#' Generate Samples from Nested Sampling
#'
#' @param x An object of class `ernest_sampler`.
#' @param max_iterations The maximum number of iterations to perform. If set to
#' Inf, this stopping criterion is ignored.
#' @param max_calls The maximum number of calls to the likelihood function.
#' If set to Inf, this stopping criterion is ignored.
#' @param min_logz The minimum log-evidence value to achieve. Be a number equal or
#' larger than zero, or `NULL`, in which case it is transformed to zero.
#' @param refresh Whether to clear existing points from the sampler, starting
#' a run from scratch.
#' @param ... Must be empty.
#'
#' @returns `x`, invisibly.
#' @details This method wraps a call to [ernest_sampler$generate()].
#' @importFrom generics generate
#' @method generate ernest_sampler
#' @export
generate.ernest_sampler = function(x,
                                   max_iterations = Inf,
                                   max_calls = Inf,
                                   min_logz = 0.05,
                                   ...) {
  check_dots_empty()
  x$generate(max_iterations, max_calls, min_logz)
}

#' Estimate the Log-Evidence of a Model
#'
#' Calculate the marginal likelihood of a given model and return the estimates
#' in a tidy [tibble()].
#'
#' @param object An `ernest_sampler` object.
#' @param include_live A logical value indicating whether to include the live
#' points in the calculation. If `FALSE`, the live points are excluded, and
#' additional information about the integration is excluded.
#' @param ... Must be empty.
#'
#' @returns A [tibble()] with the log-evidence estimates.
#' @details This method wraps a call to [ernest_sampler$generate()].
#' @importFrom generics calculate
#' @method calculate ernest_sampler
#' @export
calculate.ernest_sampler = function(x, include_live = TRUE, ...) {
  check_dots_empty()
  x$calculate(include_live)
}


