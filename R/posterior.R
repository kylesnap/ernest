#' Transform `ErnestSampler` to `posterior::draws` objects
#'
#' Transform an `ErnestSampler` object to a format supported by the
#' posterior package.
#'
#' @param x An `ErnestSampler` object, or another \R object for which
#' the methods are defined.
#' @param scale One of `original` or `unit`. If `unit`, the returned
#' `draws` object will contain points expressed in the unit hypercube, which
#' was originally used by ernest for generated sampling. If `original` (the
#' default), the returned `draws` object will contain points in the original
#' parameter space defined by the prior transformation.
#' @param inc_live Should the live points be included? Defaults to `TRUE`.
#' @param ... Arguments passed to individual methods (if applicable).
#'
#' @rdname as_draws-ernest
#' @export
as_draws.ErnestSampler <- function(x, scale = c("original", "unit"),
                                   inc_live = TRUE, ...) {
  # Draws matrix is the most natural form for the ErnestSampler object
  as_draws_matrix(
    x, scale = scale, inc_live = inc_live
  )
}

#' @rdname as_draws-ernest
#' @export
as_draws_matrix.ErnestSampler <- function(x, scale = c("original", "unit"),
                                          inc_live = TRUE, ...) {
  scale <- arg_match(scale)
  if (is.null(x$wrk) || x$wrk$n_iter == 0) {
    cli::cli_abort("No iterations have been run with this sampler.")
  }
  dead_points <- x$wrk$get_dead(scale == "unit")

  live_points <- if (inc_live) {
    if (scale == "unit") {
      x$wrk$live_units[order(x$wrk$live_lik), ]
    } else {
      x$wrk$live_points[order(x$wrk$live_lik), ]
    }
  } else {
    NULL
  }
  points <- rbind(dead_points, live_points)

  log_weight <- calculate(x)[["log_weight"]]
  if (is.null(log_weight)) {
    cli::cli_abort("No log_weight found in ErnestSampler object.")
  }
  if (!inc_live) {
    log_weight <- head(log_weight, x$wrk$n_iter)
  }
  posterior::weight_draws(
    posterior::as_draws_matrix(points),
    weights = log_weight,
    log = TRUE
  )
}

#' @rdname as_draws-ernest
#' @export
as_draws_df.ErnestSampler <- function(x, scale = c("original", "unit"),
                                         inc_live = TRUE, ...) {
  as_draws_df(
    as_draws_matrix(x, scale = scale, inc_live = inc_live, ...)
  )
}
