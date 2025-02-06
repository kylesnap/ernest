#' Likelihood-Prior Restricted Sampling within the Unit Cube
#'
#' Generate particles by naively sampling from a unit hypercube. This is a
#' simple but inefficient method of sampling from the prior distribution.
#'
#' @param max_attempts The maximum number of attempts to find a valid point during
#' region-based sampling, including during uniform sampling.
#'
#' @details
#' This sampler can be useful in a few edge cases where the number of live
#' points is small compared to the number of dimensions, or if the user wants to
#' run tests to check the sampling behaviour.
#'
#' @return An object of class `unif_cube`, which inherits from `ernest_sampler`
#'
#' @export
unif_cube <- function(max_attempts = 1e6) {
  if (max_attempts < 1000) {
    check_number_whole(max_attempts, min = 1)
    cli::cli_warn("max_attempts is less than 1000. This may result in poor sampling.")
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
    max_attempts = max_attempts,
    subclass = "unif_cube"
  )
  obj
}

#' @noRd
refresh_sampler.unif_cube <- function(sampler) {
  do.call(new_unitcube_sampler, as.list(sampler))
}

#' @noRd
propose_live.unif_cube <- function(sampler, original, min_lik) {
  res <- propose_uniform(sampler, min_lik)
  if (is_empty(res)) {
    res <- list(
      "unit" = original,
      "parameter" = sampler$prior_transform$fn(original),
      "num_call" = sampler$max_attempts
    )
    res$log_lik <- sampler$log_lik(res$parameter)
  }
  return(res)
}
