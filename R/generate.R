#' Perform nested sampling
#'
#' Execute the nested sampling loop. Iteratively replaces the worst live point
#' in a set with a point drawn from a likelihood-restricted prior sampler,
#' until a provided stopping criterion is met.
#'
#' @param x (ernest_sampler) An object of class `ernest_sampler`.
#' @inheritParams rlang::args_dots_empty
#' @param max_iterations (positive integer) The maximum number of iterations to
#' perform. If set to `Inf`, this stopping criterion is ignored.
#' @param max_calls (positive integer) The maximum number of calls to the
#' likelihood function. If set to `Inf`, this stopping criterion is ignored.
#' @param min_logz (positive double or zero) The minimum ratio between the
#' current log evidence and remaining log evidence (see Details). If set to
#' zero, this stopping criterion is ignored.
#' @inheritParams compile.ernest_sampler
#'
#' @details
#' At least one of `max_iterations`, `max_calls`, or `min_logz`  must represent
#' a non-ignored stopping criterion.
#'
#' At the iteration \eqn{i}, the remaining log evidence within the prior space
#' bound by a minimum likelihood criterion can be estimated as
#' \deqn{Z^*_i \approx L^{(max)}_i V_i} where \eqn{L^{(max)}_i}
#' is the maximum likelihood of the current live points and \eqn{V_i} is the
#' estimated remaining volume.
#'
#' This estimate can be used to define a relative stopping criterion based on
#' the log-ratio of the current estimated evidence \eqn{\hat{Z_i}} to the
#' remaining evidence, such that
#' \deqn{\delta \log(Z) = \log(\hat{Z_i} - Z^*_i) - \log(\hat{Z_i})}
#' By setting `min_logz`, you can vary the minimum log-ratio at which sampling
#' stops. Once \eqn{\delta \log(Z)} falls below this value, you can assume that
#' only a negligible fraction of the evidence remains unaccounted for in the
#' final evidence estimates.
#'
#' If `x` already contains results from previous calls to `generate()`, then
#' `generate()` will ensure that your stopping criterion have not already been
#' surpassed during previous runs.
#'
#' Control the verbosity of `generate` as it runs with the
#' `rlib_message_verbosity` option. This can be set with one of these three
#' values:
#' - `"default"`: Verbose unless the `.frequency` argument is supplied.
#' - `"verbose"`: Always verbose.
#' - `"quiet"`: Always quiet.
#'
#' @srrstats {BS2.12, BS2.13} Documentation of the `rlang` verbosity parameter.
#' @srrstats {BS4.3, BS4.4, BS4.5} Ernest defaults to using `min_logz` to
#' halt the nested sampling loop when the remaining evidence in the unexplored
#' parameter space is sufficiently small. `max_iterations` is used to prevent
#' infinite loops when `min_logz` cannot be reached.
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
#' @srrstats {BS1.3, BS1.3a, BS2.8} Describes the stopping criteria used for
#' nested sampling, and details how the results of previous runs can be
#' used as starting points for subsequent runs.
#' @method generate ernest_sampler
#' @export
generate.ernest_sampler <- function(
  x,
  ...,
  max_iterations = Inf,
  max_calls = Inf,
  min_logz = 0.05,
  seed = NA
) {
  check_dots_empty()
  x$generate(max_iterations, max_calls, min_logz, seed)
}
