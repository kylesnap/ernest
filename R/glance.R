#' Glance at an `ernest_sampler` object
#'
#' Construct a single row summary of a given ernest run object
#'
#' @param x An `ernest_sampler` object
#' @param ... Must be left empty.
#'
#' @returns A single-row [tibble::tibble()] summarizing the run.
#' @export
glance.ernest_sampler <- function(x, ...) {
  check_dots_empty(...)
  x$glance()
}
