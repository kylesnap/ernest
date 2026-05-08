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
#' @export
weights.ernest_run <- function(x, log = FALSE, ...) {
  check_dots_empty()
  weights <- x$log_weight - x$log_evidence
  if (log) {
    weights
  } else {
    exp(weights)
  }
}

#' @noRd
#' @export
weights.ernest_rcrd <- function(x, log = FALSE, ...) {
  log_w <- compute_integral(x, truncate = TRUE)
  weights <- log_w$log_weight - log_w$log_evidence
  if (log) {
    weights
  } else {
    exp(weights)
  }
}
