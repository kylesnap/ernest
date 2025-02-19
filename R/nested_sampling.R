#' Perform Nested Sampling
#'
#' @param x A function returning the log likelihood of a model given some parameters.
#' @param ... dots, babey
#' @param sampler The likelihood-restricted prior sampler to use.
#' @param verbose Whether to print progress updates to the terminal.
#' @param ... Ignored.
#'
#' @returns Lots of stuff about the run.
#' @export
nested_sampling <- function(x, num_points = 500L, n_uniform = 100L, update_int = 100L, verbose = FALSE) {
  NULL # TODO
}
