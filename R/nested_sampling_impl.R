#' Internal implementation of the nested sampling algorithm
#'
#' Performs the core nested sampling loop, updating live points, accumulating
#' evidence, and checking stopping criteria.
#'
#' @param x An `ernest_sampler` or `ernest_run` object containing the current
#' state and configuration.
#' @param max_iterations Integer. Maximum number of iterations to perform.
#' @param max_calls Integer. Maximum number of likelihood function calls
#' allowed.
#' @param min_logz Numeric. Minimum change in log-evidence (log Z) required to
#' continue sampling.
#' @param last_criterion Numeric. Log-likelihood value of the last removed
#' sample (default: -1e300).
#' @param log_vol Numeric. Current log prior volume.
#' @param log_z Numeric. Current log-evidence.
#' @param iter Integer. Current iteration.
#' @param call Integer. Current number of likelihood calls.
#' @param show_progress Logical. If TRUE, displays a progress bar during
#' sampling.
#'
#' @return A list containing the dead points and their associated metadata,
#' representing the updated state of the nested sampler.
#' @noRd
nested_sampling_impl <- function(
  x,
  max_iterations,
  max_calls,
  min_logz,
  last_criterion = -1e300,
  log_vol = 0,
  log_z = -1e300,
  iter = 0L,
  call = 0L,
  show_progress = TRUE
) {
  live_env <- x$live_points
  max_lik <- max(live_env$log_lik)
  d_log_z <- logaddexp(0, max_lik + log_vol - log_z)
  d_log_vol <- log((x$n_points + 1) / x$n_points)

  dead_unit <- vctrs::list_of(.ptype = double(x$n_points))
  dead_birth <- vctrs::list_of(.ptype = integer())
  dead_id <- vctrs::list_of(.ptype = integer())
  dead_calls <- vctrs::list_of(.ptype = integer())
  dead_log_lik <- vctrs::list_of(.ptype = double())

  i <- 1
  if (show_progress) {
    cli::cli_progress_step("Performed {i} nested samples.", spinner = TRUE)
  }
  for (i in seq(1, max_iterations - iter)) {
    # 1. Check stop conditions
    if (call > max_calls) {
      cli::cli_inform("`max_calls` surpassed ({call} > {max_calls})")
      break
    }
    max_lik <- max(live_env$log_lik)
    d_log_z <- logaddexp(0, max_lik + log_vol - log_z)
    if (d_log_z < min_logz) {
      cli::cli_inform("`min_logz` reached ({d_log_z} < {min_logz})")
      break
    }
    if (show_progress) {
      cli_progress_update()
    }

    # 2. Identify and log the worst points in the sampler
    worst_idx <- which_minn(live_env$log_lik)
    new_criterion <- live_env$log_lik[worst_idx[1]]
    if (isTRUE(all.equal(new_criterion, max_lik))) {
      cli::cli_warn(
        "Stopping run due to a likelihood plateau at {round(max_lik, 3)}."
      )
      break
    }
    dead_unit[[i]] <- live_env$unit[worst_idx[1], ]
    dead_log_lik[[i]] <- live_env$log_lik[worst_idx[1]]
    dead_birth[[i]] <- live_env$birth[worst_idx[1]]
    dead_id[[i]] <- worst_idx[1]

    # 3. Update the integration
    log_vol <- log_vol - d_log_vol
    log_d_vol <- log(0.5 * expm1(d_log_vol)) + log_vol
    log_wt <- logaddexp(new_criterion, last_criterion) + log_d_vol
    log_z <- logaddexp(log_z, log_wt)
    last_criterion <- new_criterion

    # 4. If required, update the LRPS
    if (
      call > x$first_update &&
        env_cache(x$lrps$cache, "n_call", 0L) > x$update_interval
    ) {
      x$lrps <- update_lrps(x$lrps)
    }

    # 4. Replace the worst points in live with new points
    new_unit <- if (call <= x$first_update) {
      propose(x$lrps, criteria = live_env$log_lik[worst_idx])
    } else {
      available_idx <- setdiff(seq_len(x$n_points), worst_idx)
      copy <- sample(available_idx, length(worst_idx), replace = FALSE)
      propose(
        x$lrps,
        original = live_env$unit[copy, , drop = FALSE],
        criteria = live_env$log_lik[worst_idx]
      )
    }
    live_env$log_lik[worst_idx] <- new_unit$log_lik
    live_env$unit[worst_idx, ] <- new_unit$unit
    live_env$birth[worst_idx] <- i + iter
    dead_calls[[i]] <- new_unit$n_call
    call <- call + new_unit$n_call
  }

  list(
    "dead_unit" = dead_unit,
    "dead_log_lik" = dead_log_lik,
    "dead_id" = dead_id,
    "dead_calls" = dead_calls,
    "dead_birth" = dead_birth
  )
}
