
#' Transform to draws objects.
#'
#' @param x An `ErnestRun` object.
#' @param resample If true, the returned `draws_matrix` will be reweighted
#' before being returned. If false (default), the matrix will instead be bound
#' to the appropriate importance weights.
#' @param include_live If true, the returned `draws_matrix` will include the
#' live points.
#' @param unit_scale If true, the returned `draws_matrix` will contain points
#' expressed in the unit hypercube, which was originally used for sampling.
#' @param ... Passed to `posterior::resample_draws` if `reweight` is true.
#'
NULL

#' @rdname tidy
#' @export
S7::method(as_draws, ErnestLRPS) <-
  function(x, resample = FALSE, add_live = TRUE, unit_scale = FALSE, ...) {
  points <- x@wrk$get_dead(unit_scale)

  if (add_live) {
    live_points <- if (unit_scale) {
      x@wrk$live_units
    } else {
      x@wrk$live_points
    }
    lik_ord <- order(x@wrk$live_lik)
    points <- rbind(points, live_points[lik_ord, ])
  }

  log_weight <- if (add_live) {
    tidy(x, exponentiate = FALSE)$log_weight
  } else {
    tidy(x, exponentiate = FALSE)$log_weight[1:x@n_iter]
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
