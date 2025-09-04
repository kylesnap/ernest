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
#'
#' @srrstats {BS3.1, BS3.2} As a substitute to examining the data for perfect
#' colinearity, ernest examines whether it has reached a likelihood plateau
#' in its sample of live points (a problem for estimating the log-volume
#' cascade and interpreting NS results). Ernest reports this as a warning,
#' and terminates the sampling early. This behaviour is tested.
#' @srrstats {BS4.3, BS4.4, BS4.5} Convergence in a nested sampling run is
#' defined by the amount of evidence remaining in the unintegrated prior space,
#' controlled by the `min_logz` parameter. In cases where convergence is not
#' achieved, the sampler will stop when the maximum number of iterations or
#' likelihood calls is reached, or when the log-likelihood plateau is detected.
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
  live_env <- x$run_env
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
    cli::cli_progress_step(
      "Performing nested sampling ({i} points replaced)...",
      spinner = TRUE
    )
  }
  for (i in seq(1, max_iterations - iter)) {
    # 1. Check stop conditions
    if (call > max_calls) {
      cli::cli_inform(c("v" = "`max_calls` surpassed ({call} > {max_calls})."))
      break
    }
    max_lik <- max(live_env$log_lik)
    d_log_z <- logaddexp(0, max_lik + log_vol - log_z)
    if (d_log_z < min_logz) {
      cli::cli_inform(c(
        "v" = "`min_logz` reached ({pretty(d_log_z)} < {min_logz})."
      ))
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
        "Stopping run due to a likelihood plateau at {pretty(max_lik)}."
      )
      break
    }
    dead_unit[[i]] <- live_env$unit[worst_idx, ]
    dead_log_lik[[i]] <- live_env$log_lik[worst_idx]
    dead_birth[[i]] <- live_env$birth[worst_idx]
    dead_id[[i]] <- worst_idx

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
      x$lrps <- update_lrps(x$lrps, unit = live_env$unit)
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
  if (i >= max_iterations) {
    cli::cli_inform(c("v" = "`max_iterations` reached ({i})."))
  }

  list(
    "dead_unit" = dead_unit,
    "dead_log_lik" = dead_log_lik,
    "dead_id" = dead_id,
    "dead_calls" = dead_calls,
    "dead_birth" = dead_birth
  )
}
