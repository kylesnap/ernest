#' @importFrom posterior as_draws
#' @export
posterior::as_draws

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
#' @include ernest_run-new.R
#' @export
S7::method(as_draws, ErnestRun) <-
  function(x, resample = FALSE, add_live = TRUE, unit_scale = FALSE, ...) {

    points <- if (unit_scale) {
      x@dead_points$units
    } else {
      x@dead_points$points
    }

    if (add_live) {
      lik_ord <- order(x@sampler@wrk$live_lik)
      live_points <- if (unit_scale) {
        x@sampler@wrk$live_units
      } else {
        x@sampler@wrk$live_points
      }
      points <- rbind(points, live_points[lik_ord,])
    }

    log_weight <- if (add_live) {
      vctrs::field(x@integral, "log_weight")
    } else {
      vctrs::field(x@integral, "log_weight")[1:x@n_iter]
    }
    log_weight <- log_weight - sum(log_weight)

    draws <- posterior::weight_draws(
      posterior::as_draws_matrix(points),
      log_weight
    )

    if (resample) {
      posterior::resample_draws(draws, ...)
    } else {
      draws
    }
  }
