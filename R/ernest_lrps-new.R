#' Make a likelihood-restricted prior Sampler
#'
#' This is the base class for creating a likelihood-based prior Sampler. All
#' of ernest's Samplers inherit from this superclass.
#'
#' @param log_lik A function that takes in one vector of parameters and returns
#' a scalar log-likelihood value.
#' @param prior_transform An object of class "prior_transform" used to transform
#' values in the unit hypercube values to the original parameter space.
#' @param num_dim An integer number of dimensions in the parameter space.
#' @param description A short description of the Sampler.
#' @param maxtry The maximum number of attempts to find a valid point during
#' region-based sampling, including during uniform sampling.
#' @param ... Name-value pairs for additional elements of Samplers that
#' subclass this Sampler.
#' @param subclass The subclasses of this Sampler.
#'
#' @returns A `Sampler` object, used to propose new particles under a likelihood
#' constraint.
#' @include utils.R
ErnestLRPS <- S7::new_class(
  "ErnestLRPS",
  properties = list(
    log_lik = S7::class_function,
    prior_transform = S7::class_function,
    n_dim = prop_natural(),
    n_points = prop_natural(default = 500L),
    first_update = prop_natural(include_zero = TRUE, default = 300L),
    between_update = prop_natural(include_zero = TRUE, default = 750L),
    verbose = S7::new_property(
      S7::class_logical,
      default = getOption("verbose")
    ),
    compiled = S7::new_property(
      getter = function(self) {
        !is.null(self@wrk)
      }
    ),
    wrk = NULL | S7::class_environment
  ),
  abstract = TRUE
)

#' Propose a new live point by randomly sampling within the unit hypercube.
#'
#' @param x The Sampler object, inheriting from `Sampler`
#'
#' @returns A particle, which is a list with four components: "unit", "parameter",
#' "log_lik", and "num_calls". Returns NULL if the log-likelihood cannot be
#' overcome after a million tries.
propose_uniform <- S7::new_generic("propose_uniform", "x")

S7::method(propose_uniform, ErnestLRPS) <- function(x) {
  propose_uniform_(
    x@log_lik, x@prior_transform, x@n_dim, x@wrk$worst_lik,
    getOption("max_loop", default = 1e6)
  )
}

#' Propose a point by sampling from the bounded prior distribution.
#'
#' @param Sampler The Sampler object, inheriting from `ernest_Sampler`
#' @param copy The index of the point to copy from.
#'
#' @returns A particle, which is a list with four components: "unit", "parameter",
#' "log_lik", and "num_calls". Returns NULL if the log-likelihood cannot be
#' overcome after a million tries.
#'
#' @export
propose_live <- S7::new_generic(
  "propose_live",
  "x",
  function(x, copy) {
    S7::S7_dispatch()
  }
)

S7::method(propose_live, ErnestLRPS) <- function(x, copy) {
  propose_uniform_(
    x@log_lik, x@prior_transform, x@n_dim, x@wrk$worst_lik,
    getOption("max_loop", default = 1e6)
  )
}

#' Update the bounds of a given Sampler with the current distribution
#' of live points
#'
#' @param x The current live points
#' @param live The current live points
#'
#' @returns Either a new sampler with updated bounds, OR null, in which case
#' the old sampler is used (i.e., updating does not change the sampler's behaviour)
update_sampler <-  S7::new_generic("update_sampler", "x")

S7::method(update_sampler, ErnestLRPS) <- function(x, live) {
  return(NULL)
}
