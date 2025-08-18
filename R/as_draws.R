#' Transform nested sampling runs to `draws` objects
#'
#' Try to transform an [ernest_run] to a format supported by the
#' [posterior][posterior::posterior-package] package.
#'
#' @param x An [ernest_run] object.
#' @param units Case-sensitive string. The scale in which to return the
#' sampled points:
#' * `"original"`: Points are expressed on the scale of the prior space.
#' * `"unit_cube"`: Points are expressed on the scale of the (0-1)-unit
#' hypercube.
#' @param radial Logical. Whether to return an additional column `.radial`,
#' containing the radial coordinate (i.e., the squared sum of squares) for
#' each sampled point.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' A [draws][posterior::as_draws()] object, containing the posterior samples
#' from the nested sampling run. Each samples are also bound to their importance
#' weight (in log. units).
#'
#' The exact type of the returned object depends on the function used:
#' * For `as_draws` and `as_draws_matrix`, a [posterior::draws_matrix()]
#' object, which has classes `c("draws_matrix", "draws", "matrix")`.
#' * For `as_draws_rvars`, a [posterior::draws_rvars()] object, which has
#' classes `c("draws_rvars", "draws", "list")`.
#'
#' @examples
#' # Load example run
#' library(posterior)
#' data(example_run)
#'
#' # View importance weights
#' dm <- as_draws(example_run)
#' weights(dm) |> head()
#'
#' # Summarize points after resampling
#' dm |>
#'  resample_draws() |>
#'  summarize_draws()
#'
#' # View the radial coordinate in the unit-space over the run
#' dm_rad <- as_draws_rvars(
#'  example_run,
#'  units = "unit_cube",
#'  radial = TRUE
#' )
#' plot(
#'   x = example_run$log_volume,
#'   y = draws_of(dm_rad$.radial),
#'   xlab = "Log volume",
#'   ylab = "Radial coordinate"
#' )
#' @seealso
#' * [posterior::as_draws()] describes the `draws` object.
#' * [posterior::resample_draws()] uses the log weights within ernest's output
#' to produce a weighted posterior sample.
#' @rdname as_draws
#' @export
as_draws.ernest_run <- function(
  x,
  ...,
  units = c("original", "unit_cube"),
  radial = FALSE
) {
  as_draws_matrix.ernest_run(x, ..., units = units, radial = radial)
}

#' @rdname as_draws
#' @method as_draws_matrix ernest_run
#' @export
as_draws_matrix.ernest_run <- function(
  x,
  ...,
  units = c("original", "unit_cube"),
  radial = FALSE
) {
  as_draws_matrix_(x, ..., units = units, radial = radial)
}

#' @rdname as_draws
#' @export
as_draws_rvars.ernest_run <- function(
  x,
  ...,
  units = c("original", "unit_cube"),
  radial = FALSE
) {
  posterior::as_draws_rvars(
    as_draws_matrix(x, ..., units = units, radial = radial)
  )
}

#' Convert samples to a weighted draws matrix
#'
#' @inheritParams as_draws_matrix
#' @param error_call Environment to use for error reporting.
#'
#' @srrstats {G2.3, G2.3a} Using `arg_match` to validate character input.
#'
#' @return A weighted draws matrix of class `draws_matrix`.
#' @noRd
as_draws_matrix_ <- function(x, ..., units, radial, call = caller_env()) {
  check_dots_empty(call = call)
  units <- arg_match0(
    units,
    values = c("original", "unit_cube"),
    error_call = call
  )
  check_bool(radial, call = call)

  points <- if (units == "original") x$samples else x$samples_unit
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
