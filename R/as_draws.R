#' Transform nested sampling runs to `draws` objects
#'
#' Convert an [ernest_run] to a format supported by the
#' [posterior][posterior::posterior-package] package.
#'
#' @param x An [ernest_run] object.
#' @param units Case-sensitive string. The scale for the sampled points:
#' * `"original"`: Points are on the scale of the prior space.
#' * `"unit_cube"`: Points are on the (0, 1) unit hypercube scale.
#' @param radial Logical. If `TRUE`, returns an additional column `.radial`
#' containing the radial coordinate (i.e., the Euclidean norm) for each
#' sampled point.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' A [draws][posterior::as_draws()] object containing posterior samples
#' from the nested sampling run, with importance weights (in log units).
#'
#' The returned object type depends on the function used:
#' * For `as_draws` and `as_draws_matrix`, a [posterior::draws_matrix()]
#'   object (class `c("draws_matrix", "draws", "matrix")`).
#' * For `as_draws_rvars`, a [posterior::draws_rvars()] object (class
#'   `c("draws_rvars", "draws", "list")`).
#'
#' @seealso
#' * [posterior::as_draws()] for details on the `draws` object.
#' * [posterior::resample_draws()] uses the log weights from ernest's output
#'   to produce a weighted posterior sample.
#' @examples
#' # Load example run
#' library(posterior)
#' data(example_run)
#'
#' # View importance weights
#' dm <- as_draws(example_run)
#' weights(dm) |> head()
#'
#' # Summarise points after resampling
#' dm |>
#'   resample_draws() |>
#'   summarize_draws()
#'
#' # View the radial coordinate in unit space over the run
#' dm_rad <- as_draws_rvars(
#'   example_run,
#'   units = "unit_cube",
#'   radial = TRUE
#' )
#' plot(
#'   x = example_run$log_volume,
#'   y = draws_of(dm_rad$.radial),
#'   xlab = "Log-volume",
#'   ylab = "Radial coordinate"
#' )
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

#' Convert an ernest_run to a weighted draws matrix
#'
#' Converts an [ernest_run] object to a weighted draws matrix suitable for use
#' with the posterior package.
#'
#' @param x An [ernest_run] object.
#' @param ... Additional arguments (currently unused).
#' @param units Character. The scale for the sampled points: `"original"` or
#' `"unit_cube"`.
#' @param radial Logical. If TRUE, includes a `.radial` column with the
#' Euclidean norm for each sample.
#' @param call Environment to use for error reporting.
#'
#' @return A weighted draws matrix of class `draws_matrix`, with log weights
#' attached.
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