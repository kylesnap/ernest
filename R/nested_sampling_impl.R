#' Nested Sampling Implementation
#'
#' This function performs the nested sampling algorithm to estimate the evidence
#' of a model. It iteratively updates the live points, calculates evidence,
#' and checks for stoppage criteria. The function also handles progress updates
#' and manages the live and dead points in the sampler.
#'
#' @param self An object containing the current state of the nested sampler.
#' @param private A list containing private variables and functions used by the
#' nested sampler.
#' @param max_it Integer. The maximum number of iterations to perform.
#' @param max_c Integer. The maximum number of calls to the likelihood function.
#' @param min_logz Numeric. The minimum change in the log evidence (log Z) to
#' continue sampling.
#'
#' @srrstats {G3.0} Safely compares max and min log likelihood values for
#' plateau detection.
#'
#' @return The updated `self` object with the new state of the nested sampler.
#' @keywords internal
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom cli cli_inform
#' @noRd
nested_sampling_impl <- function(
  self,
  private,
  max_it,
  max_c,
  min_logz
) {
  prev <- private$results
  iter <- prev$n_iter %||% 0L
  call <- prev$n_call %||% 0L
  log_vol <- prev$log_vol[prev$n_iter] %||% 0.0
  log_z <- prev$log_evidence[prev$n_iter] %||% -1e300
  max_lik <- max(private$live_log_lik)
  d_log_z <- logaddexp(0, max_lik + log_vol - log_z)
  last_criterion <- prev$log_lik[prev$n_iter] %||% -1e300
  d_log_vol <- log((private$n_points + 1) / private$n_points)

  dead_unit <- vctrs::list_of(.ptype = double(private$n_points))
  dead_birth <- vctrs::list_of(.ptype = integer())
  dead_id <- vctrs::list_of(.ptype = integer())
  dead_calls <- vctrs::list_of(.ptype = integer())
  dead_log_lik <- vctrs::list_of(.ptype = double())

  progress <- paste0(
    "{cli::pb_spin} Generating nested samples | ",
    "{cli::pb_current} iter. | {call} lik. calls |",
    "{d_log_z} est. log. z remaining"
  )
  verbose <- getOption("rlib_message_verbosity", "verbose") != "quiet"
  if (verbose) {
    cli_progress_bar(format = progress, clear = TRUE)
  }
  for (i in seq(1, max_it - iter)) {
    # 1. Check stop conditions
    if (call > max_c) {
      private$status <- "MAX_CALLS"
      cli_inform("`max_calls` surpassed ({call} > {max_c})")
      break
    }
    max_lik <- max(private$live_log_lik)
    d_log_z <- logaddexp(0, max_lik + log_vol - log_z)
    if (d_log_z < min_logz) {
      private$status <- "MIN_LOGZ"
      cli_inform("`min_logz` reached ({d_log_z} < {min_logz})")
      break
    }
    if (verbose) {
      cli_progress_update()
    }

    # 2. Identify and log the worst points in the sampler
    worst_idx <- which_minn(private$live_log_lik)
    new_criterion <- private$live_log_lik[worst_idx[1]]
    if (isTRUE(all.equal(new_criterion, max_lik))) {
      cli_warn(
        "Stopping run due to a likelihood plateau at {round(max_lik, 3)}."
      )
      private$status <- "PLATEAU"
      break
    }
    dead_unit[[i]] <- private$live_unit[worst_idx[1], ]
    dead_log_lik[[i]] <- private$live_log_lik[worst_idx[1]]
    dead_birth[[i]] <- private$live_birth[worst_idx[1]]
    dead_id[[i]] <- worst_idx[1]

    # 3. Update the integration
    log_vol <- log_vol - d_log_vol
    log_d_vol <- log(0.5 * expm1(d_log_vol)) + log_vol
    log_wt <- logaddexp(new_criterion, last_criterion) + log_d_vol
    log_z <- logaddexp(log_z, log_wt)
    last_criterion <- new_criterion

    # 3. If required, update the LRPS
    # If current number of calls exceeds first_update, OR
    # if first_update is already exceeded and the number of calls
    # exceeds the update_interval, then update the LRPS
    if (
      call > private$first_update &&
        private$lrps$since_update > private$update_interval
    ) {
      private$lrps <- private$lrps$update()
    }

    # 4. Replace the worst points in live with new points
    available_idx <- setdiff(seq_len(private$n_points), worst_idx)
    copy <- sample(available_idx, length(worst_idx), replace = FALSE)

    new_unit <- if (call <= private$first_update) {
      private$lrps$propose_uniform(private$live_log_lik[worst_idx])
    } else {
      private$lrps$propose_live(
        private$live_unit[copy, ],
        private$live_log_lik[worst_idx]
      )
    }
    private$live_log_lik[worst_idx] <- new_unit$log_lik
    private$live_unit[worst_idx, ] <- new_unit$unit
    private$live_birth[worst_idx] <- i + iter
    dead_calls[[i]] <- new_unit$n_call
    call <- call + new_unit$n_call
  }
  if (!(private$status %in% c("PLATEAU", "MIN_LOGZ", "MAX_CALLS"))) {
    private$status <- "MAX_ITER"
    cli_inform("`max_iterations` reached ({max_it})")
  }
  if (verbose) {
    cli_progress_done()
  }

  list(
    "dead_unit" = dead_unit,
    "dead_log_lik" = dead_log_lik,
    "dead_id" = dead_id,
    "dead_calls" = dead_calls,
    "dead_birth" = dead_birth,
    "remaining_logz" = d_log_z
  )
}
