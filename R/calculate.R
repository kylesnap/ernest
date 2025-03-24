#' Estimate the Log-Evidence of a Model
#'
#' Calculate the marginal likelihood of a given model and return the estimates
#' in a tidy [tibble()].
#'
#' @param x An object of class `ernest_sampler`.
#' @param add_points A string, either `"none"`, `"unit"`, `"parameter"`, or `"both"`.
#' If `"none"`, no additional columns are added. If `"unit"`or `"parameter"`,
#' the parameter values associated with each point are added, in their respective
#' units. If `"both"`, both the unit and parameter values are added.
#' @param add_progress Adds columns for the number of calls to the likelihood
#' function between each iteration.
#' @param ... Must be empty.
#'
#' @details
#' Nested sampling must generate a series of increasing likelihood values
#' and a series of strictly decreasing contour volume estimates. If neither of
#' these hold, ernest will warn the user and return a stripped down `tibble`
#' with no evidence estimates or log weights. In this case, the user should
#' check the behaviour of the sampler used to generate points, or report
#' the issue to the package authors.
#'
#' @returns `NULL`, if `x` does not contain any iterations. Otherwise, a `tibble`
#' with at least these columns:
#' * `.iter`: The iteration number.
#' * `log_lik`, `log_vol`: The log likelihood and estimated prior volume associated with
#' the point extracted from the live set.
#'
#' If `log_lik` and `log_vol` are well-specified (see details), then these
#' additional columns are added:
#' * `log_z`: The log-evidence estimate.
#' * `log_z_var`: The variance of the log-evidence estimate.
#' * `log_weight`: The posterior log-weight of the point.
#' * `h`: Information, reported through the Kullback–Leibler divergence.
#'
#' If `add_points` is not `"none"`, columns are added with names given by the 'ptypes'
#' argument for the sampler. Points in unit scale have names prefixed with `unit_`.
#'
#' Finally, if `add_efficiency` is `TRUE`, columns `.calls`, `.id`, and `.sampler`
#' are added, reporting the number of likelihood calls between iterations,
#' the index of the point within the live set upon removal, and the number of
#' updates to the `ernest_lrps` object, respectively.
#' @export
calculate.ernest_sampler <- function(x,
                                     add_points = c("none", "unit", "parameter", "both"),
                                     add_progress = FALSE,
                                     ...) {
  check_dots_empty()
  x$calculate(add_points, add_progress)
}
