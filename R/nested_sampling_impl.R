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
#'
#' @return The updated `self` object with the new state of the nested sampler.
#' @keywords internal
#' @noRd
nested_sampling_impl = function(self, private, max_it, max_c, min_logz) {
  iter <- self$niterations
  call <- self$ncalls
  if (iter == 0) {
    log_vol <- 0
    log_z <- -1e300
  } else {
    log_vol <- drop(tail(private$integration$log_volume, 1L))
    log_z <- drop(tail(private$integration$log_evidence, 1L))
  }
  last_criterion <- drop(tail(private$dead$log_lik, 1)) %||% -1e300
  d_log_vol <- log((private$n_points + 1) / private$n_points)

  saved_unit <- list()
  saved_point <- list()
  saved_log_lik <- list()
  saved_log_vol <- list()
  saved_log_weight <- list()
  saved_log_z <- list()

  while (iter < max_it) {
    # 1. Check stop conditions
    if (call > max_c) {
      break
    }
    d_log_z <- logaddexp(0, max(private$live$log_lik) + log_vol - log_z)
    if (d_log_z < min_logz) {
      break
    }

    # 2. Identify and log the worst points in the sampler
    iter <- iter + 1
    worst_idx <- which_minn(private$live$log_lik)
    saved_unit[[iter]] <- private$live$unit[worst_idx[1], ]
    saved_point[[iter]] <- private$live$point[worst_idx[1], ]
    saved_log_lik[[iter]] <- private$live$log_lik[worst_idx[1]]

    # 3. Update the integration
    new_criterion <- private$live$log_lik[worst_idx[1]]
    log_vol <- log_vol - d_log_vol
    log_d_vol <- log(0.5 * expm1(d_log_vol)) + log_vol
    log_wt <- logaddexp(new_criterion, last_criterion) + log_d_vol
    log_z <- logaddexp(log_z, log_wt)
    saved_log_vol[[iter]] <- log_vol
    saved_log_weight[[iter]] <- log_wt
    saved_log_z[[iter]] <- log_z
    last_criterion <- new_criterion

    # 3. If required, update the LRPS
    # If current number of calls exceeds first_update, OR
    # if first_update is already exceeded and the number of calls
    # exceeds the update_interval, then update the LRPS
    if (call > private$first_update && private$lrps$since_update > private$update_interval) {
      private$lrps <- private$lrps$update()
    }

    # 4. Replace the worst points in live with new points
    # TODO: Parallelize this!
    for (idx in seq_along(worst_idx)) {
      copy <- sample.int(private$n_points, 1)
      while (copy == worst_idx[idx]) {
        copy <- sample.int(private$n_points, 1)
      }
      new <- private$lrps$propose_live(
        private$live$unit[copy, ],
        new_criterion
      )
      if (is_empty(new$unit)) {
        cli::cli_abort(
          "Region-based sampler couldn't improve the worst point."
        )
      }
      private$live$unit[worst_idx[idx],] <- new$unit
      private$live$point[worst_idx[idx],] <- new$parameter
      private$live$log_lik[worst_idx[idx]] <- new$log_lik
    }
    call <- call + new$num_calls
  }

  private$dead$unit <- rbind(
    private$dead$unit,
    do.call(rbind, saved_unit)
  )
  private$dead$point <- rbind(
    private$dead$point,
    do.call(rbind, saved_point)
  )
  private$dead$log_lik <- c(
    private$dead$log_lik,
    list_c(saved_log_lik)
  )
  private$integration <- rbind(
    private$integration,
    tibble::tibble(
      "log_volume" = list_c(saved_log_vol),
      "log_weight" = list_c(saved_log_weight),
      "log_evidence" = list_c(saved_log_z)
    )
  )

  private$n_iter <- iter
  private$n_call <- call
  invisible(self)
}
