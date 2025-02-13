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
nested_sampling <- function(x, num_points = 500, n_uniform = 100, update_int = 100, verbose = FALSE) {
  run <- ErnestRun$new(x, num_points, n_uniform, update_int, verbose)
}
