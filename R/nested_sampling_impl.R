#' Nested Sampling Implementation
#'
#' This function performs the nested sampling algorithm to estimate the evidence
#' of a model. It iteratively updates the live points, calculates evidence,
#' and checks for stoppage criteria. The function also handles progress updates
#' and manages the live and dead points in the sampler.
#'
#' @param live_env A live points enviroment.
#' @param lrps An LRPS object to generate new points.
#' @param max_iterations Integer. The maximum number of iterations to perform.
#' @param max_c Integer. The maximum number of calls to the likelihood function.
#' @param min_logz Numeric. The minimum change in the log evidence (log Z) to
#' continue sampling.
#'
#' @srrstats {G3.0} Safely compares max and min log likelihood values for
#' plateau detection.
#'
#' @return The updated `self` object with the new state of the nested sampler.
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom cli cli_inform
#' @noRd
nested_sampling_impl <- function(
  x,
  max_iterations,
  max_calls,
  min_logz,
  last_criterion = -1e300,
  log_vol = 0,
  log_z = 0,
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
      cli::cli_inform("`max_calls` surpassed ({call} > {max_c})")
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
    if (call > x$first_update && x$lrps$since_update > x$update_interval) {
      x$lrps <- x$lrps$update()
    }

    # 4. Replace the worst points in live with new points
    available_idx <- setdiff(seq_len(x$n_points), worst_idx)
    copy <- sample(available_idx, length(worst_idx), replace = FALSE)

    new_unit <- if (call <= x$first_update) {
      x$lrps$propose_uniform(live_env$log_lik[worst_idx])
    } else {
      x$lrps$propose_live(
        live_env$unit[copy, ],
        live_env$log_lik[worst_idx]
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
