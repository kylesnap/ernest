#' Unit Cube Likelihood-Prior Sampling
#'
#' Generate particles by naively sampling from a unit hypercube. This is a
#' simple but inefficient method of sampling from the prior distribution.
#'
#' @param num_dim The number of particles to propose.
#'
#' @details
#' This sampler can be useful in a few edge cases where the number of live
#' points is small compared to the number of dimensions, or if the user wants to
#' run tests to check the sampling behaviour.
#'
#' @return An object of class `unit_cube`, which inherits from `sampler`
#'
#' @export
unit_cube <- function(num_dim) {
  obj <- new_unit_cube(num_dim = num_dim)
  validate(obj)
}

#' Class constructor
#' @noRd
new_unit_cube <- function(log_lik = NULL, prior = NULL,
                          name = "Unit Cube Sampler",
                          num_dim = NULL) {
  obj <- new_sampler(
    log_lik = log_lik,
    prior = prior,
    name = name,
    num_dim = num_dim,
    subclass = "unit_cube"
  )
  obj
}

#' @noRd
refresh.unit_cube <- function(sampler) {
  obj <- do.call(new_unit_cube, as.list(sampler))
  validate(obj)
}

#' @export
#' @rdName lrps
lrps.unit_cube <- function(sampler, u_point, log_lik_criterion, ...) {
  for (i in 1:1e7) {
    u_point <- runif(sampler$num_dim)
    point <- sampler$prior(u_point)
    log_lik <- sampler$log_lik(point)
    if (is.finite(log_lik) && log_lik > log_lik_criterion) {
      return(
        list(u_point = u_point, point = point, log_lik = log_lik, num_call = i)
      )
    }
  }
  rlang::abort("Failed to find a valid point after 1e7 iterations")
}
