#' Likelihood-Prior Restricted Sampling with A Random Walk within the Unit Cube
#'
#' Generate particles by randomly walking away from an existing live point.
#' The walk reflects off of the bounds of the unit cube.
#'
#' @param steps The minimum number of steps to take during the random walk
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
#' @include sampler.R
#' @export
RandomWalkCube <- S7::new_class(
  "RandomWalkCube",
  properties = list(
    steps = S7::new_property(
      S7::class_integer,
      default = 20L
    ),
    epsilon = S7::new_property(
      S7::class_numeric,
      default = 0.1
    )
  ),
  parent = Sampler,
  validator = function(self) {
    if (self@steps < 1L) {
      return("@steps should be an integer greater or equal to one.⁠")
    }
    if (self@maxtry <= 0) {
      return("@epsilon should be a number greater than zero.⁠")
    }
  }
)

S7::method(propose_live, RandomWalkCube) <- function(x, start, min_lik) {
  propose_rwcube_(
    x@log_lik, x@prior_transform, start, min_lik, x@steps, x@epsilon
  )
}
