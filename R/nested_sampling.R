#' Build a Nested Sampler
#'
#' Constructs an instance of an `ernest_sampler` containing the necessary
#' information for performing nested sampling.
#'
#' @param log_lik The log-likelihood function. This should take in a vector of
#' parameters and return a scalar numeric value that is either finite or `-Inf`.
#' @param prior An `ernest_prior` object, created by [`create_prior()`] or
#' its specializations.
#' @param sampler An [ernest_sampling] object, declaring which likelihood-restricted
#' prior sampler to use for the nested sampling run.
#' @param n_points The number of live points to use in the nested sampling run.
#' @param first_update The number of calls to the likelihood function
#' before the first update of the sampler. If left as a double, this will be
#' multiplied by the number of live points before being coerced to an
#' integer.
#' @param update_interval The number of calls to the likelihood function
#' between subsequent updates of the sampler. If left as a double, this will
#' be multiplied by the number of live points before being coerced to an
#' integer.
#'
#' @return An `ernest_sampler` object.
#' @export
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
    stop_input_type(prior, "ernest_prior")
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
