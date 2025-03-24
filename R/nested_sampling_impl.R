#' Nested Sampling Implementation
#' 
#' This function performs the nested sampling algorithm to estimate the evidence (log Z) of a model. 
#' It iteratively updates the live points, calculates the log evidence, and checks for convergence criteria.
#' The function also handles progress updates and manages the live and dead points in the sampler.
#' 
#' @param self An object containing the current state of the nested sampler.
#' @param private A list containing private variables and functions used by the nested sampler.
#' @param max_it Integer. The maximum number of iterations to perform.
#' @param max_c Integer. The maximum number of calls to the likelihood function.
#' @param min_logz Numeric. The minimum change in the log evidence (log Z) to continue sampling.
#' @return The updated `self` object with the new state of the nested sampler.
#' @keywords internal
#' @noRd
nested_sampling_impl = function(self, private, max_it, max_c, min_logz) {
  iter <- self$n_iterations
  calls <- self$n_calls
  log_z <- private$.last_log_z
  log_vol <- unlist(tail(private$.log_vol, 1)) %||% 0
  d_log_vol <- log((private$.n_points + 1) / private$.n_points)
  worst_log_lik <- unlist(tail(private$.dead$log_lik, 1)) %||% -1e300
  num_updates <- 0

  saved_units <- list()
  saved_points <- list()
  saved_log_lik <- list()
  saved_progress <- list()

  if (self$verbose) {
    cli::cli_progress_bar(
      type = "custom",
      format = "{cli::pb_spin} Nested Sampling | ln(z): {.val {log_z}}"
    )
  }

  status <- NULL
  for (iter in seq2(iter + 1, max_it)) {
    # Check kill criteria
    if (calls > max_c) {
      if (self$verbose) cli::cli_progress_done()
      status <- "`max_calls` reached"
      break
    }
    d_log_z <- logaddexp(
      0,
      max(private$.live$log_lik) + log_vol - log_z
    )
    if (d_log_z < min_logz) {
      if (self$verbose) cli::cli_progress_done()
      status <- "`min_logz` reached"
      break
    }
    if (self$verbose && (iter %% 10 == 0)) cli::cli_progress_update()

    # Find and remove the worst point in the live sampler
    worst_idx <- which.min(private$.live$log_lik)
    saved_units[[iter]] <- private$.live$units[worst_idx, ]
    saved_points[[iter]] <- private$.live$points[worst_idx, ]
    saved_log_lik[[iter]] <- private$.live$log_lik[worst_idx]
    new_worst_lik <- private$.live$log_lik[worst_idx]

    # Constrict the prior volume and push an update to the integral
    log_vol <- log_vol - d_log_vol
    log_d_vol <- log(0.5 * expm1(d_log_vol)) + log_vol
    log_wt <- logaddexp(new_worst_lik, worst_log_lik) + log_d_vol
    log_z <- logaddexp(log_z, log_wt)
    private$.log_vol[[iter]] <- log_vol
    worst_log_lik <- new_worst_lik

    # Update the LRPS
    if ((private$.lrps$update_interval != 0) &&
        (private$.since_update >= private$.lrps$update_interval)) {
      private$.lrps <- update_sampler(private$.lrps)
      private$.since_update <- 0
      num_updates <- num_updates + 1
    }

    # Create a new point and push it to the live set
    copy <- sample.int(private$.n_points, size = 1)
    while (copy == worst_idx) {
      copy <- sample.int(private$.n_points, size = 1)
    }
    new <- propose_live(
      private$.lrps,
      private$.live$unit[copy, ],
      worst_log_lik
    )
    if (is_empty(new$unit)) {
      cli::cli_abort(
        "Region-based sampler couldn't improve the worst point."
      )
    }
    private$.live$units[worst_idx, ] <- new$unit
    private$.live$points[worst_idx, ] <- new$parameter
    private$.live$log_lik[worst_idx] <- new$log_lik

    saved_progress[[iter]] <- list(
      ".calls" = new$num_calls,
      ".id" = worst_idx,
      "sampler" = num_updates
    )

    calls <- calls + new$num_calls
    private$.since_update <- private$.since_update + new$num_calls
  }
  if (self$verbose) cli::cli_progress_done()
  private$.dead <- push_dead_points(
    private$.dead,
    saved_units,
    saved_points,
    saved_log_lik
  )
  private$.progress <- push_progress(
    private$.progress,
    saved_progress
  )
  private$.last_log_z <- log_z
  invisible(self)
}
