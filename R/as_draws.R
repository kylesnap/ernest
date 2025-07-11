#' Transform nested sampling runs to `draws` objects
#'
#' Try to transform an [ernest_run-class] to a format supported by the
#' [posterior][posterior::posterior-package] package.
#'
#' @param x An `ernest_run` object.
#' @inheritParams rlang::args_dots_empty
#' @param units Which units to express the sampled points. If `"original"`,
#' points are expressed on the scale of the prior space. If `"unit_cube"`,
#' points are expressed on the scale of the (0-1)-unit hypercube.
#' @param radial Whether to return an additional column `.radial`, containing
#' the radial coordinate (i.e., the squared sum of squares) for each sampled
#' point.
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
#' data(ernest_run_example)
#'
#' # View importance weights
#' dm <- as_draws(ernest_run_example)
#' weights(dm) |> head()
#'
#' # Summarize points after resampling
#' dm |>
#'  resample_draws() |>
#'  summarize_draws()
#'
#' # View the radial coordinate in the unit-space over the run
#' dm_rad <- as_draws_rvars(ernest_run_example, units = "unit_cube", radial = TRUE)
#' plot(
#'   x = ernest_run_example$log_volume,
#'   y = draws_of(dm_rad$.radial),
#'   xlab = "Log volume",
#'   ylab = "Radial coordinate"
#' )
#' @rdname as_draws.ernest
#' @method as_draws ernest_run
#' @export
as_draws.ernest_run <- function(
  x,
  ...,
  units = c("original", "unit_cube"),
  radial = FALSE
) {
  as_draws_matrix.ernest_run(x, ..., units = units, radial = radial)
}

#' @rdname as_draws.ernest
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

#' @rdname as_draws.ernest
#' @method as_draws_rvars ernest_run
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

#' Internal function for `as_draws_matrix` conversion
#' @noRd
as_draws_matrix_ <- function(x, ..., units, radial, error_call = caller_env()) {
  check_dots_empty(call = error_call)
  units <- arg_match0(units, values = c("original", "unit_cube"), error_call)
  check_bool(radial, error_call = error_call)

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
