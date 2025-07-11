#' Prepare a nested sampler to evaluate Bayesian evidence
#'
#' Initialize a [ernest_sampler] object to perform nested sampling with a given
#' log-likelihood function, prior distribution, and likelihood-restricted prior
#' specification.
#'
#' @param log_lik Either an `ernest_likelihood` or a function that takes in a vector
#' of parameters and returns the corresponding log. likelihood. This parameter is
#' sent to [create_likelihood()].
#' @param prior An `ernest_prior` object, created by [create_prior()] or
#' its [specializations][create_special_prior].
#' @param sampler An [ernest_sampling] object, declaring which likelihood-restricted
#' prior sampler to use.
#' @param n_points The number of live points to use in the nested sampling run.
#' @param first_update The number of calls to the likelihood function
#' before the first update to the behaviour of `sampler`. If left as a double,
#' `first_update` is set to `first_update * n_points`.
#' @param update_interval The number of calls to the likelihood function
#' between updates to `sampler` after `first_update` has been reached. If left
#' as a double, `update_interval` is set to `update_interval * n_points`.
#'
#' @return An [ernest_sampler] object.
#' @export
#' @examples
#' prior <- create_uniform_prior(n_dim = 2, lower = -1, upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- nested_sampling(ll_fn, prior, n_points = 100)
#' sampler
nested_sampling <- function(
  log_lik,
  prior,
  sampler = rwmh_cube(),
  n_points = 500,
  first_update = 2.5,
  update_interval = 1.5
) {
  loglik <- create_likelihood(log_lik)
  if (!inherits(prior, "ernest_prior")) {
    stop_input_type(prior, "an ernest_prior object")
  }
  ernest_sampler$new(
    loglik,
    prior,
    sampler,
    n_points,
    first_update,
    update_interval
  )
}
