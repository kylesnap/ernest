#' Unit Cube Likelihood-Prior Sampling
#'
#' Generate particles by naively sampling from a unit hypercube. This is a
#' simple but inefficient method of sampling from the prior distribution.
#'
#' @details
#' This sampler can be useful in a few edge cases where the number of live
#' points is small compared to the number of dimensions, or if the user wants to
#' run tests to check the sampling behaviour.
#'
#' @return An object of class `unit_cube`, which inherits from `ernest_sampler`
#'
#' @export
unit_cube <- function(max_attempts = 1e6) {
  if (max_attempts < 1000) {
    cli::cli_warning("max_attempts is less than 1000. This may result in poor sampling.")
  }
  new_unitcube_sampler(max_attempts = max_attempts)
}

new_unitcube_sampler <- function(log_lik = NULL, prior_transform = NULL,
                                 num_dim = 0,
                                 name = "Unit Cube Sampler",
                                 description = "Uses the unit hypercube to bound the set of live points.",
                                 max_attempts = 1e6,
                                 ...) {
  obj <- new_sampler(
    log_lik = log_lik,
    prior_transform = prior_transform,
    num_dim = num_dim,
    name = name,
    description = description,
    subclass = "unit_cube"
  )
  obj
}

#' @noRd
refresh_sampler.unit_cube <- function(sampler) {
  do.call(new_unitcube_sampler, as.list(sampler))
}

#' @noRd
propose_live.unit_cube <- function(sampler, original, min_lik) {
  propose_uniform(sampler, min_lik)
}
