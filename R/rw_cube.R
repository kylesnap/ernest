#' Likelihood-Prior Restricted Sampling with A Random Walk within the Unit Cube
#'
#' Generate particles by randomly walking away from an existing live point.
#' The walk reflects off of the bounds of the unit cube.
#'
#' @param steps The number of steps to take in the random walk
#' @param epsilon The initial step size of the random walk. This is refined throughout
#' the run to target a 0.5 acceptance rate.
#'
#' @details
#' This sampler is based off the original approach adopted by John Skilling.
#' A metropolis-hastings random walk is conducted from a given live point,
#' where each step is accepted based whether on its evaluated likelihood is better
#' compared against the likelihood criterion.
#'
#' @return An object of class `rw_cube`, which inherits from `ernest_sampler`
#'
#' @export
rw_cube <- function(steps = 20, epsilon = 0.1) {
  new_rwcube_sampler(steps = steps, epsilon = epsilon)
}

new_rwcube_sampler <- function(log_lik = NULL, prior_transform = NULL,
                               num_dim = 0,
                               name = "Random Walk Sampling within the Unit Cube",
                               description = "Performs a metropolis-hastings walk to evolve a live point.",
                               steps = NULL,
                               epsilon = NULL,
                               ...) {
  check_number_whole(steps, min = 1)
  check_number_decimal(epsilon, min = 0.001)
  obj <- new_sampler(
    log_lik = log_lik,
    prior_transform = prior_transform,
    num_dim = num_dim,
    name = name,
    description = description,
    steps = steps,
    epsilon = epsilon,
    subclass = "rw_cube"
  )
  obj
}

#' @export
#' @noRd
refresh_sampler.rw_cube <- function(sampler) {
  do.call(new_rwcube_sampler, as.list(sampler))
}

#' @export
#' @noRd
propose_live.rw_cube <- function(sampler, original, min_lik) {
  propose_rwcube_(
    sampler$log_lik, sampler$prior_transform$fn, original,
    min_lik, sampler$steps, sampler$epsilon
  )
}
