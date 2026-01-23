#' Transform nested sampling runs to `draws` objects
#'
#' Access the posterior sample and weights from a nested sampling run as an
#' object supported by the [posterior][posterior::posterior-package] package.
#'
#' @param x [`[ernest_run]`][ernest_run]\cr Results from a nested sampling run.
#' @param units `[character(1)]`\cr The scale of the sampled points:
#' * `"original"`: Points are on the scale of the prior space.
#' * `"unit_cube"`: Points are on the (0, 1) unit hypercube scale.
#' @param radial `[logical(1)]`\cr If `TRUE`, returns an additional column
#' `.radial` containing the radial coordinate (i.e., the Euclidean norm) for
#' each sampled point.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns [posterior::draws_matrix()] or [posterior::draws_rvars()]\cr
#' A  object containing the posterior samples from the nested sampling run,
#' with a hidden `.weights` column containing the importance weights for each
#' sample.
#'
#' @note To produce a weighted posterior sample, use
#' [posterior::resample_draws()] to reweigh an object from `as_draws` using its
#' importance weights.
#'
#' @srrstats {G2.3, G2.3a, G2.3b} Uses arg_match() to ensure an informative
#' error message is provided when the user provides an invalid value for
#' `radial`.
#'
#' @seealso
#' * [posterior::as_draws()] for details on the `draws` object.
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
#' @export
as_draws.ernest_run <- function(
  x,
  units = c("original", "unit_cube"),
  radial = FALSE,
  ...
) {
  as_draws_matrix.ernest_run(x, ..., units = units, radial = radial)
}

#' @rdname as_draws.ernest_run
#' @export
as_draws_matrix.ernest_run <- function(
  x,
  units = c("original", "unit_cube"),
  radial = FALSE,
  ...
) {
  as_draws_matrix_(x, ..., units = units, radial = radial)
}

#' @rdname as_draws.ernest_run
#' @export
as_draws_rvars.ernest_run <- function(
  x,
  units = c("original", "unit_cube"),
  radial = FALSE,
  ...
) {
  posterior::as_draws_rvars(
    as_draws_matrix(x, ..., units = units, radial = radial)
  )
}

#' Convert an ernest_run to a weighted draws matrix
#'
#' @param x An ernest_run object.
#' @param ... Additional arguments (currently unused).
#' @param units Character. The scale for the sampled points: "original" or
#'   "unit_cube".
#' @param radial Logical. If TRUE, includes a .radial column with the
#'   Euclidean norm for each sample.
#' @param call Environment to use for error reporting.
#'
#' @return A draws_matrix object with importance weights attached.
#' @noRd
as_draws_matrix_ <- function(x, ..., units, radial, call = caller_env()) {
  check_dots_empty(call = call)
  units <- arg_match0(
    units,
    values = c("original", "unit_cube"),
    error_call = call
  )
  check_bool(radial, call = call)

  points <- x$samples[[units]]
  if (radial) {
    radial_col <- sqrt(rowSums(points^2))
    points <- cbind(points, ".radial" = radial_col)
  }
  weights <- x$weights$imp_weight

  posterior::weight_draws(
    posterior::as_draws_matrix(points),
    weights,
    log = FALSE
  )
}
