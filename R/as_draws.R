#' Transform an Ernest Run into a `draws` object
#'
#' Transform an `ErnestSampler` object into format supported by the
#' posterior package. This allows for the easy analysis of the posterior
#' distribution generated through nested sampling.
#'
#' @param x An `ErnestSampler` object.
#' @param resample If true, the returned `draws_matrix` will be reweighted
#' before being returned. If false (default), the matrix will instead be bound
#' to the appropriate importance weights.
#' @param add_live If true, the returned `draws_matrix` will include the
#' live points.
#' @param unit_scale If true, the returned `draws_matrix` will contain points
#' expressed in the unit hypercube, which was originally used for sampling.
#' @param ... Passed to `posterior::resample_draws` if `reweight` is true.
#'
#' @returns A `draws_matrix` object, which is a 2D array containing
#' `x$n_dim` "variables" and `x$iter` (plus `x$n_point` if `include_live` is
#' true) "draws".
#'
#' @seealso [posterior::as_draws_matrix()], [posterior::resample_draws()],
#' and [posterior::weight_draws()] for more information on the returned object.
#'
#' @exportS3Method posterior::as_draws
as_draws.ErnestSampler <- function(x, resample = FALSE, add_live = TRUE,
                                   unit_scale = FALSE, ...) {
  if (is.null(x$wrk)) {
    cli::cli_abort("No iterations have been run with this sampler.")
  }
  points <- x$wrk$get_dead(unit_scale)

  if (add_live) {
    live_points <- if (unit_scale) {
      x$wrk$live_units
    } else {
      x$wrk$live_points
    }
    lik_ord <- order(x$wrk$live_lik)
    points <- rbind(points, live_points[lik_ord, ])
  }

  log_weight <- if (add_live) {
    calculate(x, exponentiate = FALSE)$log_weight
  } else {
    calculate(x, exponentiate = FALSE)$log_weight[1:x$wrk$n_iter]
  }

  draws <- posterior::weight_draws(
    posterior::as_draws_matrix(points),
    log_weight,
    log = TRUE
  )

  if (resample) {
    posterior::resample_draws(draws, ...)
  } else {
    draws
  }
}
