#' Propose a new live point by sampling uniformly within the unit hypercube.
#'
#' @param x An `ernest_lrps` object
#' @param criterion The log-likelihood criterion to use.
#'
#' @return A list representing a particle, containing the following elements:
#' * `unit`: A vector of length `n_dim` representing the point in the hypercube
#' * `parameters`: A vector of length `n_dim` representing the point in the
#' parameter space
#' * `log_lik`: The log-likelihood of the point
#' * `num_calls`: The number of calls to the log-likelihood function needed to
#' generate a point that satisfies `criterion`. If `criterion` is -Inf, then
#' this is guaranteed to be equal to one if the sampler always generates
#' points with finite or `-Inf` log-likelihood.
#'
#' @noRd
propose_uniform <- function(x, ...) {
  UseMethod("propose_uniform")
}

#' Method for Ernest Sampler
#' @noRd
#' @export
propose_uniform.ernest_lrps <- function(x, criterion = -Inf, ...) {
  lrps_uniform(
    x$log_lik,
    x$prior_transform,
    x$n_dim,
    criterion,
    getOption("ernest.max_loop", default = 1e6)
  )
}

#' Propose a new particle through likelihood-restricted prior sampling.
#'
#' @param x An `ErnestSampler` object
#' @param original A point in the parameter space to start sampling from
#' @param criterion The log-likelihood criterion to use for LRPS.
#'
#' @returns A list with at-least the following components:
#' * `unit`: A vector of length `n_dim` representing the point in the hypercube
#' * `parameters`: A vector of length `n_dim` representing the point in the
#' parameter space
#' * `log_lik`: The log-likelihood of the point
#' * `num_calls`: The number of calls to the log-likelihood function needed to
#' generate the point.
#'
#' @noRd
propose_live <- function(x, ...) {
  UseMethod("propose_live")
}

#' @noRd
#' @export
propose_live.ernest_lrps <- function(x, original, criterion, ...) {
  lrps_uniform(
    x$log_lik,
    x$prior_transform,
    x$n_dim,
    criterion,
    getOption("ernest.max_loop", default = 1e6)
  )
}

#' Method for Random Cube
#' The CPP method called here will also return "num_acc" in the list,
#' which is the number steps where the proposal was accepted.
#' @noRd
#' @export
propose_live.rw_cube <- function(x, original, criterion, ...) {
  new <- lrps_rwcube(
    log_lik = x$log_lik,
    prior_transform = x$prior_transform,
    original = original,
    criterion = criterion,
    steps = x$num_steps,
    epsilon = tail(x$epsilon, 1)[[1]]
  )
  env_poke(x$args, "n_acc", new$num_acc)
  env_poke(x$args, "n_steps", new$num_steps)
  new
}
