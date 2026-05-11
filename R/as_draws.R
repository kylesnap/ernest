#' Transform nested sampling runs to `draws` objects
#'
#' Access the posterior sample and weights from a nested sampling run as an
#' object supported by the [posterior][posterior::posterior-package] package.
#'
#' @param x [`[ernest_run]`][ernest_run]\cr Results from a nested sampling run.
#' @param units `[character(1)]`\cr The scale of the sampled points:
#' * `"original"`: Points are on the scale of the prior space.
#' * `"unit_cube"`: Points are on the (0, 1) unit hypercube scale.
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
#' @seealso [posterior::as_draws()]
#'
#' @examples
#' library(posterior)
#' data(example_run)
#'
#' # View importance weights
#' dm <- as_draws(example_run)
#' str(dm)
#' weights(dm) |> head()
#'
#' # Summarise points after resampling
#' dm |>
#'   resample_draws() |>
#'   summarize_draws()
#'
#' # Extract the same coordinates in the unit space coordinates
#' dm_unit <- as_draws_rvars(example_run, units = "unit_cube")
#' str(dm_unit)
#' @export
as_draws.ernest_run <- function(
  x,
  units = c("original", "unit_cube"),
  ...
) {
  as_draws_matrix.ernest_run(x, units = units, ...)
}

#' @rdname as_draws.ernest_run
#' @export
as_draws_rvars.ernest_run <- function(
  x,
  units = c("original", "unit_cube"),
  ...
) {
  posterior::as_draws_rvars(
    as_draws_matrix(x, ..., units = units)
  )
}

#' @rdname as_draws.ernest_run
#' @export
as_draws_matrix.ernest_run <- function(
  x,
  units = c("original", "unit_cube"),
  ...
) {
  check_dots_empty()
  units <- arg_match(units)

  points <- field(x$rcrd, "unit")
  if (units == "original") {
    points <- x$prior$fn(points)
  }
  colnames(points) <- x$prior$names
  posterior::weight_draws(
    posterior::as_draws_matrix(points),
    weights = weights(x, log = TRUE),
    log = TRUE
  )
}
