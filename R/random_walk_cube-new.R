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
    steps = prop_natural(include_zero = FALSE, default = 20L),
    epsilon = S7::new_property(
      S7::class_numeric,
      default = 0.1
    )
  ),
  parent = ErnestLRPS
)

#' @name format
#' @export
S7::method(format, RandomWalkCube) <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cli::cli_format_method({
      cli::cli_dl(c(
        "Sampler" = "Random Walk in the Unit Hypercube",
        sampler_info,
        "Steps" = x@steps,
        "Epsilon" = x@epsilon
      ))
    })
  }

S7::method(propose_live, RandomWalkCube) <- function(x, copy) {
  propose_rwcube_(
    x@log_lik, x@prior_transform, x@wrk$live_units[copy, ],
    x@wrk$worst_lik, x@steps, x@epsilon
  )
}
