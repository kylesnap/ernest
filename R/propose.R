#' Propose a new particle by sampling uniformly within the unit hypercube.
#'
#' All `ErnestSampler` samplers will initially use this method to propose new
#' points until the number of calls to `log_lik` exceeds `first_update.` This
#' method also serves as the default fallback for `propose_live` if the
#' subclass of the chosen LRPS does not provide a specialization for
#' `propose_live`.
#'
#' @param x An `ErnestSampler` object
#'
#' @return A list representing a particle, with components "unit",
#' "parameters", "log_lik", and "num_calls".
#' @noRd
propose_uniform <- function(x) {
  UseMethod("propose_uniform")
}

#' Method for Ernest Sampler
#' @noRd
#' @export
propose_uniform.ErnestSampler <- function(x) {
  propose_uniform_(
    x$log_lik, x$prior_transform, x$n_dim, x$wrk$worst_lik,
    getOption("ernest.max_loop", default = 1e6)
  )
}

#' Propose a new particle through likelihood-restricted prior sampling.
#'
#' @param x An `ErnestSampler` object
#' @param copy An integer, showing the location of the copied parameter
#'
#' @return A list representing a particle, with components "unit",
#' "parameters", "log_lik", and "num_calls"
#' @noRd
#' @export
propose_live <- function(x, copy) {
  UseMethod("propose_live")
}

#' Method for Ernest Sampler
#' @noRd
#' @export
propose_live.ErnestSampler <- function(x, copy) {
  propose_uniform_(
    x$log_lik, x$prior_transform, x$n_dim, x$wrk$worst_lik,
    getOption("ernest.max_loop", default = 1e6)
  )
}

#' Method for Random Cube
#' @noRd
#' @export
propose_live.RandomWalkCube <- function(x, copy) {
  propose_rwcube_(
    x@log_lik, x@prior_transform, x@wrk$live_units[copy, ],
    x@wrk$worst_lik, x@steps, x@epsilon
  )
}
