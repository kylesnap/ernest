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
unit_cube <- function() {
  new_unitcube_sampler()
}

new_unitcube_sampler <- function(log_lik = NULL, prior_transform = NULL,
                                 num_dim = 0,
                                 name = "Unit Cube Sampler",
                                 description = "Uses the unit hypercube to bound the set of live points.",
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
  num_call <- 0L
  for (i in 1:1e6) {
    proposed_unit <- stats::runif(sampler$num_dim)
    proposed_point <- sampler$prior_transform$fn(proposed_unit)
    proposed_ll <- sampler$log_lik(proposed_point)
    if (proposed_ll > min_lik) {
      return(list(
        "unit" = proposed_unit,
        "parameter" = proposed_point,
        "log_lik" = proposed_ll,
        "num_call" = num_call
      ))
    }
  }
  cli::cli_abort("Failed to find a valid point after 1e6 iterations.")
}
