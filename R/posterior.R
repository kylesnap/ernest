#' Convert Nested Samples to `draws` Objects
#'
#' Transform the points stored in an ``ernest_draws` object into a format
#' supported by the `posterior` package.
#'
#' @param x An `ernest_draws` object.
#' @inheritParams rlang::args_dots_empty
#' @param unit_cube Whether to return the points in their unit cube coordinates.
#' If `FALSE` (default), the points are returned in the original prior space.
#' @param radial Whether to return an additional column `.radial`, containing
#' the radial coordinate (i.e., the squared sum of squares) for each sampled
#' point.
#'
#' @return A `draws_matrix` object containing the points and their associated
#' log weights.
#'
#' @rdname as_draws.ernest
#' @importFrom posterior as_draws
#' @method as_draws ernest_run
#' @export
#' @export as_draws
as_draws.ernest_run <- function(x, ..., unit_cube = FALSE, radial = FALSE) {
  as_draws_matrix.ernest_run(x, ..., unit_cube = unit_cube, radial = radial)
}

#' @importFrom posterior as_draws_matrix
#' @method as_draws_matrix ernest_run
#' @rdname as_draws.ernest
#' @export
#' @export as_draws_matrix
as_draws_matrix.ernest_run <- function(x, ..., unit_cube = FALSE, radial = FALSE) {
  check_dots_empty()

  points <- if (unit_cube) x$samples_unit else x$samples
  if (radial) {
    radial_col <- sqrt(rowSums(points^2))
    points <- cbind(points, ".radial" = radial_col)
  }
  weights <- x$log_weight - max(x$log_evidence)

  posterior::weight_draws(
    posterior::as_draws_matrix(points),
    weights,
    log = TRUE
  )
}

#' @importFrom posterior as_draws_rvars
#' @method as_draws_rvars ernest_run
#' @rdname as_draws.ernest
#' @export
#' @export as_draws_rvars
as_draws_rvars.ernest_run <- function(x, ...) {
  posterior::as_draws_rvars(as_draws_matrix(x, ...))
}
