#' Unit Cube Likelihood-Prior Sampling
#'
#' Generate particles by naively sampling from a unit hypercube. This is a
#' simple but inefficient method of sampling from the prior distribution.
#'
#' @param queue_size The number of particles to prepare in batches.
#'
#' @details
#' This sampler can be useful in a few edge cases where the number of live
#' points is small compared to the number of dimensions, or if the user wants to
#' run tests to check the sampling behaviour.
#'
#' @return An object of class `unit_cube`, which inherits from `ernest_sampler`
#'
#' @export
unit_cube <- function(queue_size = 20) {
  new_unitcube_sampler(queue_size = 20)
}

new_unitcube_sampler <- function(log_lik = NULL, prior_transform = NULL,
                                 num_dim = 0,
                                 queue_size = 20,
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
  obj$queue_size <- queue_size
  obj$queue_units <- matrix(runif(queue_size * obj$num_dim), ncol = obj$num_dim)
  obj$queue_points <- matrix(nrow = queue_size, ncol = num_dim)
  obj$queue_ll <- rep(-Inf, queue_size)
  obj
}

#' @noRd
refresh_sampler.unit_cube <- function(sampler) {
  do.call(new_unitcube_sampler, as.list(sampler))
}

#' @noRd
propose_live.unit_cube <- function(sampler, original, min_lik) {
  num_call <- 0L
  while (TRUE) {
    candidates <- which(sampler$queue_ll > min_lik)
    if (length(candidates > 0)) {
      break
    }
    sampler$queue_units <- matrix(runif(sampler$queue_size * sampler$num_dim), ncol = sampler$num_dim)
    sampler$queue_points <- t(apply(sampler$queue_units, 1, sampler$prior_transform$fn))
    sampler$queue_ll <- apply(sampler$queue_points, 1, sampler$log_lik)
    num_call <- num_call + sampler$queue_size
  }
  rand_i <- candidates[sample.int(length(candidates), size = 1)]
  new_ll <- sampler$queue_ll[rand_i]
  sampler$queue_ll[rand_i] <- -Inf
  list(
    "unit" = sampler$queue_units[rand_i, ],
    "parameter" = sampler$queue_points[rand_i, ],
    "log_lik" = new_ll,
    "num_call" = num_call
  )
}
