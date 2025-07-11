#' Perform nested sampling
#'
#' Execute the nested sampling loop. Iteratively replaces the worst live point
#' in a set with a point drawn from a likelihood-restricted prior sampler,
#' until a provided stopping criterion is met.
#'
#' @param x An object of class `ernest_sampler`.
#' @inheritParams rlang::args_dots_empty
#' @param max_iterations The maximum number of iterations to perform. If set to
#' `Inf`, this stopping criterion is ignored.
#' @param max_calls The maximum number of calls to the likelihood function.
#' If set to `Inf`, this stopping criterion is ignored.
#' @param min_logz The minimum log-evidence value to achieve. Must be a number
#' equal to or larger than zero.
#' @param verbose Whether to print updates on the sampler's progress.
#'
#' @details Before starting the sampling loop, `generate` calls [compile()] to
#' check the internal state of the sampler.
#'
#' @returns An object of class [ernest_run-class], containing the results of the
#' nested sampling run.
#'
#' @examples
#' prior <- create_uniform_prior(n_dim = 2, lower = -1, upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- nested_sampling(ll_fn, prior, n_point = 100)
#' sampler
#'
#' # Stop sampling after a set number of iterations or calls to the lik. func.
#' generate(sampler, max_iterations = 100)
#'
#' # The final number of calls may be larger than `max_calls`, as `generate`
#' # only checks the number of calls when removing a live point.
#' generate(sampler, max_calls = 2600)
#'
#' # Use the default stopping criteria
#' \dontrun{ generate(sampler) }
#' @method generate ernest_sampler
#' @export
generate.ernest_sampler <- function(
  x,
  ...,
  max_iterations = Inf,
  max_calls = Inf,
  min_logz = 0.05,
  verbose = FALSE
) {
  check_dots_empty()
  x$generate(max_iterations, max_calls, min_logz, verbose)
}
