#' Propose a new particle by sampling uniformly within the unit hypercube.
#'
#' All `ErnestLRPS` samplers will initially use this method to propose new
#' points until the number of calls to `log_lik` exceeds `first_update.` This
#' method also serves as the default fallback for `propose_live` if the
#' subclass of the chosen LRPS does not provide a specialization for
#' `propose_live`.
#'
#' @param x An `ErnestLRPS` object
#'
#' @return A list representing a particle, with components "unit",
#' "parameters", "log_lik", and "num_calls".
propose_uniform <- new_generic("propose_uniform", "x")

method(propose_uniform, ErnestLRPS) <- function(x) {
  propose_uniform_(
    x@log_lik, x@prior_transform, x@n_dim, x@wrk$worst_lik,
    getOption("max_loop", default = 1e6)
  )
}

#' Propose a new particle through likelihood-restricted prior sampling.
#'
#' @param x An `ErnestLRPS` object
#' @param copy An integer, showing the location of the copied parameter
#'
#' @return A list representing a particle, with components "unit",
#' "parameters", "log_lik", and "num_calls"
propose_live <- S7::new_generic(
  "propose_live",
  "x",
  function(x, copy) {
    S7::S7_dispatch()
  }
)

method(propose_live, ErnestLRPS) <- function(x, copy) {
  propose_uniform_(
    x@log_lik, x@prior_transform, x@n_dim, x@wrk$worst_lik,
    getOption("max_loop", default = 1e6)
  )
}

method(propose_live, RandomWalkCube) <- function(x, copy) {
  propose_rwcube_(
    x@log_lik, x@prior_transform, x@wrk$live_units[copy, ],
    x@wrk$worst_lik, x@steps, x@epsilon
  )
}
