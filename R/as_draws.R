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
#' plot(x = draws_of(dm_rad$.radial))
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
  c(points, weights) %<-%
    as_draws_matrix_(x, ..., units = units, radial = radial)
  posterior::weight_draws(
    posterior::as_draws_matrix(points),
    weights,
    log = TRUE
  )
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
#' @return A list with objects "points" and "weights", which are
#' guaranteed to be the same size.
#' @noRd
as_draws_matrix_ <- function(x, ..., units, radial, call = caller_env()) {
  check_dots_empty(call = call)
  units <- arg_match0(
    units,
    values = c("original", "unit_cube"),
    error_call = call
  )
  check_bool(radial, call = call)

  points <- field(x$rcrd, "unit")
  if (units == "original") {
    points <- x$prior$fn(points)
  }
  colnames(points) <- x$prior$names
  if (radial) {
    radial_col <- sqrt(rowSums(points^2))
    points <- cbind(points, ".radial" = radial_col)
  }
  vctrs::df_list(
    "points" = points,
    "weights" = weights(x, log = TRUE),
    .error_call = call
  )
}

#' Extract the posterior importance weights from a nested sampling run
#'
#' Return the normalised importance weights for the dead points in a nested
#' sampling run. On the log scale, these are `log_weight - log_evidence`; on
#' the probability scale, they are exponentiated so they sum to one.
#'
#' @param x [[ernest_run]]\cr A nested sampling run.
#' @param log `[logical(1)]`\cr Whether the weights should be returned on the
#' log scale.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns `[double()]` A numeric vector of normalised importance weights.
#'
#' @details
#' The log-weights in a nested sampling run are the individual contributions
#' of each sample to the log-evidence estimate. They are calculated from the
#' sample's log-likelihood and the amount of prior volume inside its likelihood
#' contour.
#'
#' The posterior importance weights are obtained by normalising the log-weights
#' with the final log-evidence estimate. They can be used to reweight the
#' posterior samples from the run so they approximate the posterior
#' distribution.
#'
#' @seealso [as_draws.ernest_run] to extract the posterior samples from a nested
#' sampling run, bound to their corresponding importance weights.
#'
#' @exportS3Method stats::weights
#' @examples
#' data(example_run)
#' weights(example_run) |> head()
#' weights(example_run, log = TRUE) |> head()
weights.ernest_run <- function(x, log = FALSE, ...) {
  check_dots_empty()
  weights <- x$log_weight - x$log_evidence
  if (log) {
    weights
  } else {
    exp(weights)
  }
}
