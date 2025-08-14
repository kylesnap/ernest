#' Prepare a nested sampler to evaluate Bayesian evidence
#'
#' Initialize a [ernest_sampler] object to perform nested sampling with a given
#' log-likelihood function, prior distribution, and likelihood-restricted prior
#' specification.
#'
#' @srrstatsTODO {G2.6}
#'
#' @param log_lik (function) A function or `ernest_likelihood` that takes a
#' vector of parameters and returns the log-likelihood. The function should
#' return a finite value or `-Inf` for invalid parameters
#' (see [create_likelihood()]).
#' @param prior (ernest_prior) An object with class `ernest_prior`, created by
#' [create_prior()] or its specializations.
#' @param sampler (ernest_sampling) An [ernest_sampling] object, declaring which
#' likelihood-restricted prior sampler (LRPS) to use.
#' @param n_points (positive integer) The number of live points to use in the
#' nested sampling run.
#' @param first_update (optional positive integer) The number of likelihood
#' calls before adopting LRPS behaviour. If `NULL`, this is set to
#' `n_points * 2.5`.
#' @param update_interval (optional positive integer) The number of likelihood
#' calls between updates to the LRPS behaviour. If `NULL`, this is set to
#' `n_points * 1.5`.
#' @inheritDotParams create_likelihood -fn
#'
#' @return An [ernest_sampler] object, prepared for nested sampling.
#'
#' @details
#' This function prepares an `ernest_sampler` object for performing nested
#' sampling, given a log-likelihood function, prior, and sampler specification.
#' It checks that the provided log-likelihood and prior are compatible by
#' performing a basic sanity check by evaluating the log-likelihood at the
#' center of the prior space. This check ensures that the log-likelihood
#' returns a finite value or `-Inf` for typical parameter values.
#'
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
  first_update = NULL,
  update_interval = NULL,
  ...
) {
  new_ernest_sampler(
    log_lik_fn = create_likelihood(log_lik, ...),
    prior = prior,
    sampling = sampler,
    n_points = n_points,
    first_update = first_update %||% as.integer(n_points * 2.5),
    update_interval = update_interval %||% as.integer(n_points * 1.5)
  )
}
