#' The nested sampling loop.
#'
#' @param sampler An `ErnestSampler` object.
#' @param max_it The maximum number of iterations needed to run the sampler.
#' @param max_call Maximum number of calls to the likelihood function.
#' @param dlogz The threshold for the remaining prior volume to the total.
#'
#' @returns The sampler, which may have been updated.
#'
#' @noRd
nested_sampling_impl <- function(sampler, max_it, max_call, dlogz) {
  wrk <- sampler$wrk
  iter <- wrk$n_iter
  calls <- wrk$n_call
  log_vol <- wrk$log_vol
  log_z <- wrk$log_z
  cur_update <- wrk$last_update
  n_since_update <- 0L
  d_log_vol <- log((sampler$n_points + 1) / sampler$n_points)

  for (iter in c(1:max_it)) {
    if (calls > max_call) {
      status <- "MAX_CALL"
      # if (env$verbose) cli::cli_progress_done()
      break
    }
    d_log_z <- logaddexp(0, max(wrk$live_lik) + log_vol - log_z)
    if (d_log_z < dlogz) {
      status <- "MIN_EVID"
      # if (env$verbose) cli::cli_progress_done()
      break
    }

    # Get the worst point and constrict the prior volume
    worst_idx <- wrk$worst_idx
    new_worst_lik <- wrk$live_lik[worst_idx]
    log_vol <- log_vol - d_log_vol

    # Update the integration
    log_d_vol <- log(0.5 * expm1(d_log_vol)) + log_vol
    log_wt <- logaddexp(new_worst_lik, wrk$worst_lik) + log_d_vol
    log_z <- logaddexp(log_z, log_wt)

    # Kill the worst point and mark a point to be used as replacement
    copy <- wrk$pop_point(iter, log_z, log_vol)

    # Increment the sampler if required
    if (cur_update == 0L && calls > sampler$first_update) {
      cur_update <- 1L
      n_since_update <- 0L
      sampler <- update_sampler(sampler) %||% sampler
    }
    if (n_since_update > sampler$between_update) {
      cur_update <- cur_update + 1L
      n_since_update <- 0L
      sampler <- update_sampler(sampler) %||% sampler
    }

    # Create a new point
    new <- if (cur_update == 0L) {
      propose_uniform(sampler)
    } else {
      propose_live(sampler, copy)
    }

    # Save the new point over the dead point, and increment the calls counter
    calls <- calls + wrk$push_point(iter, new, copy, cur_update)
  }
  sampler
}
