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
#' @param name A name for the given Sampler.
#' @param description A short description of the Sampler.
#' @param maxtry The maximum number of attempts to find a valid point during
#' region-based sampling, including during uniform sampling.
#' @param ... Name-value pairs for additional elements of Samplers that
#' subclass this Sampler.
#' @param subclass The subclasses of this Sampler.
#'
#' @returns A `Sampler` object, used to propose new particles under a likelihood
#' constraint.
Sampler <- S7::new_class(
  "Sampler",
  properties = list(
    log_lik = S7::class_function,
    prior_transform = S7::class_function,
    num_dim = S7::class_integer,
    maxtry = S7::new_property(
      S7::class_integer,
      default = 1e6L
    )
  ),
  validator = function(self) {
    if (!identical(self@num_dim, integer()) && self@num_dim < 1L) {
      return("@num_dim should be either NULL or an integer greater or equal to one.⁠")
    }
    if (self@maxtry < 1L) {
      return("@maxtry should be an integer greater or equal to one.⁠")
    }
    NULL
  },
  abstract = TRUE
)

#' Propose a new live point by randomly sampling within the unit hypercube.
#'
#' @param x The Sampler object, inheriting from `Sampler`
#' @param min_lik The log-likelihood criterion for the new particle.
#'
#' @returns A particle, which is a list with four components: "unit", "parameter",
#' "log_lik", and "num_calls". Returns NULL if the log-likelihood cannot be
#' overcome after a million tries.
#'
#' @export
propose_uniform <-  S7::new_generic(
  "propose_uniform",
  "x",
  function(x, min_lik) {
    S7::S7_dispatch()
  }
)

S7::method(propose_uniform, Sampler) <- function(x, min_lik) {
  propose_uniform_(
    x@log_lik, x@prior_transform, x@num_dim, min_lik, x@maxtry
  )
}

#' Propose a point by sampling from the bounded prior distribution.
#'
#' @param Sampler The Sampler object, inheriting from `ernest_Sampler`
#' @param original The original position of the live point.
#' @param min_lik The log-likelihood criterion for the new particle.
#'
#' @returns A particle, which is a list with four components: "unit", "parameter",
#' "log_lik", and "num_calls". Returns NULL if the log-likelihood cannot be
#' overcome after a million tries.
#'
#' @export
propose_live <- S7::new_generic(
  "propose_live",
  "x",
  function(x, start, min_lik) {
    S7::S7_dispatch()
  }
)

S7::method(propose_live, Sampler) <- function(x, start, min_lik) {
  propose_uniform_(
    x@log_lik, x@prior_transform, x@num_dim, min_lik, x@maxtry
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
update_sampler <-  S7::new_generic(
  "update_sampler",
  "x",
  function(x, live) {
    S7::S7_dispatch()
  }
)

S7::method(update_sampler, Sampler) <- function(x, live) {
  return(NULL)
}
