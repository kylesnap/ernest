#' Internal implementation of the nested sampling algorithm
#'
#' Executes the core nested sampling loop, iteratively updating the live set,
#' accumulating evidence, and checking stopping criteria. This function is
#' responsible for removing the lowest-likelihood live point, updating the
#' evidence estimate, and proposing new points until convergence or a stopping
#' condition is met.
#'
#' @param x An `ernest_sampler` or `ernest_run` object containing the current
#'   state and configuration.
#' @param max_iterations Integer. Maximum number of iterations to perform.
#' @param max_evaluations Integer. Maximum number of likelihood function
#' evaluations.
#' @param min_logz Numeric. Minimum change in log-evidence (log Z) required to
#'   continue sampling.
#' @param last_criterion Numeric. Log-likelihood value of the last removed
#'   sample (default: -1e300).
#' @param log_vol Numeric. Current log prior volume.
#' @param log_z Numeric. Current log-evidence.
#' @param curiter Integer. Current iteration.
#' @param cureval Integer. Current number of likelihood calls.
#' @param show_progress Logical. If `TRUE`, displays a progress bar during
#'   sampling.
#'
#' @return
#' A list containing the dead points and their associated metadata:
#' * `dead_unit`: List of unit-space coordinates of removed points.
#' * `dead_log_lik`: List of log-likelihood values for removed points.
#' * `dead_id`: List of indices of removed points.
#' * `dead_evals`: List of likelihood evaluations used for each replacement.
#' * `dead_birth`: List of birth log-likelihoods for removed points.
#'
#' @srrstats {BS3.1, BS3.2} As a substitute to examining the data for perfect
#' colinearity, ernest examines whether it has reached a likelihood plateau
#' in its live set (a problem for estimating the log-volume
#' cascade and interpreting NS results). Ernest reports this as a warning,
#' and terminates the sampling early. This behaviour is tested.
#' @srrstats {BS4.3, BS4.4, BS4.5} Convergence in a nested sampling run is
#' defined by the amount of evidence remaining in the unintegrated prior space,
#' controlled by the `min_logz` parameter. In cases where convergence is not
#' achieved, the sampler will stop when the maximum number of iterations or
#' likelihood calls is reached, or when the log-likelihood plateau is detected.
#'
#' @importFrom cli pb_spin pb_elapsed pb_current col_green symbol
#' @importFrom prettyunits pretty_signif
#' @noRd
nested_sampling_impl <- function(
  x,
  max_iterations,
  max_evaluations,
  min_logz,
  last_criterion = -1e300,
  log_vol = 0,
  log_z = -1e300,
  curiter = 0L,
  cureval = 0L,
  show_progress = TRUE
) {
  live_env <- x$run_env
  max_lik <- max(live_env$log_lik)
  d_log_z <- logaddexp(0, max_lik + log_vol - log_z)
  d_log_vol <- log((x$nlive + 1) / x$nlive)
  initial_update <- FALSE

  dead_unit <- vctrs::list_of(.ptype = double(x$nlive))
  dead_birth <- vctrs::list_of(.ptype = double())
  dead_id <- vctrs::list_of(.ptype = integer())
  dead_evals <- vctrs::list_of(.ptype = integer())
  dead_log_lik <- vctrs::list_of(.ptype = double())

  i <- 1
  if (show_progress) {
    cli::cli_progress_bar(
      format = paste0(
        "{pb_spin} Generating samples | {pb_current} iter. | {cureval} ",
        "log-lik. calls | {pretty_signif(d_log_z)} log-evid. remaining"
      ),
      type = "custom"
    )
  }
  for (i in seq(1, max_iterations - curiter)) {
    # 1. Check stop conditions
    if (cureval > max_evaluations) {
      break
    }
    max_lik <- max(live_env$log_lik)
    d_log_z <- logaddexp(0, max_lik + log_vol - log_z)
    if (d_log_z < min_logz) {
      break
    }
    if (show_progress) {
      cli::cli_progress_update()
    }

    # 2. Identify and log the worst points in the sampler
    worst_idx <- which.min(live_env$log_lik)
    new_criterion <- live_env$log_lik[worst_idx]
    if (isTRUE(all.equal(new_criterion, max_lik))) {
      cli::cli_warn(
        "Stopping run due to a likelihood plateau at {max_lik}."
      )
      break
    }
    dead_unit[[i]] <- live_env$unit[worst_idx, ]
    dead_log_lik[[i]] <- live_env$log_lik[worst_idx]
    dead_birth[[i]] <- live_env$birth_lik[worst_idx]
    dead_id[[i]] <- worst_idx

    # 3. Update the integration
    log_vol <- log_vol - d_log_vol
    log_d_vol <- log(0.5 * expm1(d_log_vol)) + log_vol
    log_wt <- matrixStats::logSumExp(c(new_criterion, last_criterion)) +
      log_d_vol
    log_z <- matrixStats::logSumExp(c(log_z, log_wt))
    last_criterion <- new_criterion

    # 4. If required, update the LRPS
    if (!initial_update && cureval >= x$first_update) {
      x$lrps <- update_lrps(x$lrps, unit = live_env$unit, log_volume = log_vol)
      initial_update <- TRUE
    }
    if (initial_update && (x$lrps$cache$neval %||% 0L) > x$update_interval) {
      x$lrps <- update_lrps(x$lrps, unit = live_env$unit, log_volume = log_vol)
    }

    # 4. Replace the worst points in live with new points
    available_idx <- setdiff(seq_len(x$nlive), worst_idx)
    copy <- sample(available_idx, length(worst_idx), replace = FALSE)
    new_unit <- if (cureval <= x$first_update) {
      propose(x$lrps, criterion = live_env$log_lik[worst_idx])
    } else {
      propose(
        x$lrps,
        original = live_env$unit[copy, ],
        criterion = live_env$log_lik[worst_idx]
      )
    }
    if (is.null(new_unit$unit)) {
      cli::cli_warn(
        "LRPS failed to generate a point in {x$lrps$max_loop} attempts."
      )
      break
    }
    live_env$log_lik[worst_idx] <- new_unit$log_lik
    live_env$unit[worst_idx, ] <- new_unit$unit
    live_env$birth_lik[worst_idx] <- copy
    dead_evals[[i]] <- new_unit$neval
    cureval <- cureval + new_unit$neval
  }

  list(
    "dead_unit" = dead_unit,
    "dead_log_lik" = dead_log_lik,
    "dead_id" = dead_id,
    "dead_evals" = dead_evals,
    "dead_birth" = dead_birth
  )
}
