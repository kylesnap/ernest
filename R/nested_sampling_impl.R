#' Internal implementation of the nested sampling algorithm
#'
#' Executes the core nested sampling loop, iteratively updating the live set,
#' accumulating evidence, and checking stopping criteria. This function is
#' responsible for removing the lowest-likelihood live point, updating the
#' evidence estimate, and proposing new points until convergence or a stopping
#' condition is met.
#'
#' @param live_env The environment containing the current live set.
#' @param lrps The likelihood-restricted prior sampler.
#' @param sampler_info A list containing information about the sampler.
#' @param control parameters for the nested sampling run, generated from
#' `set_run_control()`.
#' @param show_progress Logical. If `TRUE`, displays a progress bar during
#' sampling.
#'
#' @return
#' A list containing the dead points and their associated metadata:
#' * `dead_unit`: Matrix of unit-space coordinates of removed points.
#' * `dead_log_lik`: Vector of log-likelihood values for removed points.
#' * `dead_id`: Vector of indices of removed points.
#' * `dead_evals`: Vector of likelihood evaluations used for each replacement.
#' * `dead_birth`: Vector of birth log-likelihoods for removed points.
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
#' @noRd
nested_sampling_impl <- function(
  live_env,
  lrps,
  sampler_info,
  control,
  show_progress = TRUE
) {
  preserve_seed(sampler_info$seed)
  max_lik <- max(live_env$log_lik)
  log_vol <- control$log_vol
  log_z <- control$log_z
  last_criterion <- control$last_criterion
  nlive <- sampler_info$nlive
  plateau <- 0L
  cur_eval <- control$cur_eval
  d_log_z <- matrixStats::logSumExp(0, max_lik + log_vol - log_z)
  initial_update <- FALSE

  dead_unit <- vctrs::list_of(.ptype = double(lrps$n_dim))
  dead_birth <- vctrs::list_of(.ptype = double())
  dead_id <- vctrs::list_of(.ptype = integer())
  dead_evals <- vctrs::list_of(.ptype = integer())
  dead_log_lik <- vctrs::list_of(.ptype = double())

  i <- 1
  if (show_progress) {
    cli::cli_progress_bar(
      format = paste0(
        "{pb_spin} Generating samples | {pb_current} iter. | {cur_eval} ",
        "log-lik. calls | {signif(d_log_z)} log-evid. remaining"
      ),
      type = "custom"
    )
  }
  for (i in seq(1, control$max_iterations - control$cur_iter)) {
    # 1. Check stop conditions
    if (cur_eval > control$max_evaluations) {
      break
    }
    max_lik <- max(live_env$log_lik)
    d_log_z <- logspace_add_c(0, max_lik + log_vol - log_z)
    if (d_log_z < control$min_logz) {
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
    plateau <- if (new_criterion == last_criterion) plateau + 1L else 0L
    d_log_vol <- log((nlive + 1 - plateau) / (nlive - plateau))
    log_vol <- log_vol - d_log_vol
    log_d_vol <- log(0.5 * expm1(d_log_vol)) + log_vol
    log_wt <- matrixStats::logSumExp(c(new_criterion, last_criterion)) +
      log_d_vol
    log_z <- matrixStats::logSumExp(c(log_z, log_wt))
    last_criterion <- new_criterion

    # 4. If required, update the LRPS
    if (!initial_update && cur_eval >= sampler_info$first_update) {
      lrps <- update_lrps(lrps, unit = live_env$unit, log_volume = log_vol)
      initial_update <- TRUE
    }
    if (
      initial_update &&
        (lrps$cache$neval %||% 0L) > sampler_info$update_interval
    ) {
      lrps <- update_lrps(lrps, unit = live_env$unit, log_volume = log_vol)
    }

    # 5. Replace the worst points in live with new points
    copy <- sample.int(nlive, 1)
    if (copy == worst_idx && nlive > 1) {
      copy <- sample.int(nlive, 1)
    }
    new_unit <- if (cur_eval <= sampler_info$first_update) {
      propose(lrps, criterion = live_env$log_lik[worst_idx])
    } else {
      propose(
        lrps,
        original = live_env$unit[copy, ],
        criterion = live_env$log_lik[worst_idx]
      )
    }
    if (is.null(new_unit$unit)) {
      cli::cli_warn(
        "LRPS failed to generate a point in {lrps$max_loop} attempts."
      )
      break
    }
    live_env$log_lik[worst_idx] <- new_unit$log_lik
    live_env$unit[worst_idx, ] <- new_unit$unit
    live_env$birth_lik[worst_idx] <- live_env$log_lik[worst_idx]
    dead_evals[[i]] <- new_unit$neval
    cur_eval <- cur_eval + new_unit$neval
  }

  new_ernest_rcrd(
    unit = do.call(rbind, dead_unit),
    log_lik = vctrs::vec_c(!!!dead_log_lik, .ptype = double()),
    id = vctrs::vec_c(!!!dead_id, .ptype = double()),
    nlive = get_points(
      vctrs::vec_c(!!!dead_log_lik, .ptype = double()),
      nlive,
      FALSE
    ),
    evals = vctrs::vec_c(!!!dead_evals, .ptype = double()),
    birth_lik = vctrs::vec_c(!!!dead_birth, .ptype = double())
  )
}
