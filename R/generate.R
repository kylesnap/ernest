#' Generate nested samples from an `ernest_sampler` object.
#'
#' @param x An `ernest_sampler` object.
#' @param max_iterations The maximum number of iterations needed to run the
#' sampler. Must be a number larger than zero.
#' @param max_calls Maximum number of calls to the likelihood function. Must
#' be a number larger than zero.
#' @param min_logz The threshold for the remaining prior volume to the total
#' evidence. Must represent a number larger or equal to zero.
#' @param refresh  If `TRUE`, and if the `ernest_sampler` already contains
#' a list of live points, the existing live points will be overwritten.
#' @param ... Must be left blank.
#'
#' @returns `x`, invisibly.
#' @export
generate.ernest_sampler <- function(x,
                                    max_iterations = Inf,
                                    max_calls = Inf,
                                    min_logz = 0.05,
                                    refresh = FALSE,
                                    ...) {
  check_dots_empty()
  x$generate(
    max_iterations = max_iterations,
    max_calls = max_calls,
    min_logz = min_logz,
    refresh = refresh
  )
}
