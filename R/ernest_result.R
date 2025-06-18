#' Results from Nested Sampling Runs
#'
#' The `ernest_run` object contains the results of a nested sampling run,
#' including posterior samples, evidence estimates, and run diagnostics.
#'
#' @name ernest_run-class
#' @aliases ernest_run
#' @docType class
#'
#' @slot n_iter Integer. Number of iterations (number of dead points).
#' @slot n_points Integer. Number of live points at the end of the run.
#' @slot n_calls Integer. Total number of likelihood calls made.
#' @slot log_lik Numeric vector. Log-likelihood values for all samples (dead and live points).
#' @slot log_volume Numeric vector. Log-volume associated with each sample.
#' @slot log_weight Numeric vector. Log-posterior weights for each sample.
#' @slot log_evidence Numeric vector. Cumulative log-evidence estimates at each iteration.
#' @slot log_evidence_var Numeric vector. Cumulative uncertainty (variance) of log-evidence estimates.
#' @slot information Numeric vector. Information integral (H) at each iteration.
#' @slot id Integer vector. Index of each sample.
#' @slot points Integer vector. Number of live points at each iteration.
#' @slot calls Integer vector. Number of likelihood calls for each sample.
#' @slot birth Integer vector. Iteration at which each sample was born.
#' @slot samples Matrix. Posterior samples in parameter space.
#' @slot samples_unit Matrix. Posterior samples in unit cube coordinates.
#'
#' @details
#' The `ernest_run` object is returned by running a nested sampling procedure in the `ernest` package.
#' It can be used for posterior analysis, evidence estimation, and diagnostic plotting.
#'
NULL

#' Internal method for cosntructing the ernest_run object using an ernest_sampler
#' @importFrom vctrs vec_c vec_size
#' @noRd
compile_results <- function(
  self,
  private,
  dead_unit,
  dead_log_lik,
  dead_id,
  dead_calls,
  dead_birth
) {
  prev <- private$results
  live_loc <- -1 * attr(prev, "live_loc")
  dead_unit <- rbind(
    prev$samples_unit[live_loc, ],
    do.call(rbind, dead_unit)
  )
  dead_log_lik <- c(prev$log_lik[live_loc], list_c(dead_log_lik))
  dead_id <- c(prev$id[live_loc], list_c(dead_id))
  dead_calls <- c(prev$calls[live_loc], list_c(dead_calls))
  dead_birth <- c(prev$birth[live_loc], list_c(dead_birth))
  n_dead <- vec_size(dead_log_lik)

  live_order <- order(private$live_log_lik)
  n_live <- vec_size(live_order)

  samples_unit <- rbind(dead_unit, private$live_unit[live_order, ])
  colnames(samples_unit) <- private$prior$varnames
  samples <- t(apply(samples_unit, 1, private$prior$fn))
  colnames(samples) <- private$prior$varnames

  log_lik <- vec_c(
    dead_log_lik,
    private$live_log_lik[live_order],
    .ptype = double()
  )
  id <- vec_c(dead_id, live_order, .ptype = integer())
  points <- vec_c(
    rep(private$n_points, n_dead),
    seq(private$n_points, 1, -1),
    .ptype = integer()
  )
  calls <- vec_c(dead_calls, rep(0L, n_live), .ptype = integer())
  birth <- vec_c(dead_birth, private$live_birth[live_order], .ptype = integer())

  res <- vctrs::df_list(
    "samples" = samples,
    "samples_unit" = samples_unit,
    "log_lik" = log_lik,
    "id" = id,
    "points" = points,
    "calls" = calls,
    "birth" = birth
  )

  new_ernest_run(res, n_live, n_dead, live_order)
}

new_ernest_run <- function(res, n_live, n_dead, live_order) {
  log_vol <- cumsum(-1 * (res$points ** -1))
  integration <- compute_integral(res$log_lik, log_vol)
  live_loc <- live_order + n_dead

  structure(
    list2(
      "n_iter" = n_dead,
      "n_points" = n_live,
      "n_calls" = sum(res$calls),
      !!!integration,
      "id" = res$id,
      "points" = res$points,
      "calls" = res$calls,
      "birth" = res$birth,
      "samples" = res$samples,
      "samples_unit" = res$samples_unit
    ),
    "live_loc" = live_loc,
    class = "ernest_run"
  )
}

#' @export
format.ernest_run <- function(x, ...) {
  log_z <- formatC(tail(x$log_evidence, 1), digits = 4, format = "fg")
  log_z_sd <- formatC(
    sqrt(tail(x$log_evidence_var, 1)),
    digits = 4,
    format = "fg"
  )
  cli::cli_format_method({
    cli::cli_h1("Ernest Nested Sampling Run")
    cli::cli_dl(c(
      "No. Live Points" = "{x$n_points}",
      "No. Iterations" = "{x$n_iter}",
      "No. Lik. Calls" = "{x$n_calls}",
      "Log. Evidence (\U00B1 Err.)" = "{log_z} (\U00B1 {log_z_sd})"
    ))
  })
}

#' @export
print.ernest_run <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' Summarise a nested sampling run
#'
#' Provides a summary of an `ernest_run` object, including cumulative likelihood calls,
#' log-likelihood, log-volume, log-weights (relative to the final evidence), cumulative
#' log-evidence, evidence error, and information at each iteration.
#'
#' @param object An `ernest_run` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class `summary.ernest_run`, a list with:
#' * `n_iter`: Number of iterations (number of dead points).
#' * `n_points`: Number of live points at the end of the run.
#' * `log_volume`: Final log-volume.
#' * `log_evidence`: Final log-evidence estimate.
#' * `log_evidence_err`: Final log-evidence error (standard deviation).
#' * `run`, A tibble with columns:
#' * * `call`: The cumulative number of likelihood calls at each iteration.
#' * * `log_weight`: Log-posterior weights for each sample, now normalised
#' by the final log-evidence estimate.
#' * * `log_lik`, `log_volume`, `log_evidence`, `log_evidence_err` and
#' `information`: Run results as in [ernest_run].
#'
#' @export
summary.ernest_run <- function(object, ...) {
  check_dots_empty()
  log_z_max <- tail(object$log_evidence, 1)

  sum_df <- tibble::tibble(
    "call" = cumsum(object$call),
    "log_lik" = object$log_lik,
    "log_volume" = object$log_volume,
    "log_weight" = object$log_weight - log_z_max,
    "log_evidence" = object$log_evidence,
    "log_evidence_err" = sqrt(object$log_evidence_var),
    "information" = object$information
  )

  structure(
    list(
      "n_iter" = object$n_iter,
      "n_points" = object$n_points,
      "n_calls" = object$n_calls,
      "log_volume" = tail(sum_df$log_volume, 1),
      "log_evidence" = tail(sum_df$log_evidence, 1L),
      "log_evidence_err" = tail(sum_df$log_evidence_err, 1L),
      "run" = sum_df
    ),
    class = "summary.ernest_run"
  )
}

#' @export
format.summary.ernest_run <- function(x, ...) {
  log_z <- formatC(x$log_evidence, digits = 4, format = "fg")
  log_z_sd <- formatC(x$log_evidence_err, digits = 4, format = "fg")

  cli::cli_format_method({
    cli::cli_h1("Ernest Nested Sampling Run Summary")
    cli::cli_dl(c(
      "No. Points" = "{x$n_points}",
      "No. Iterations" = "{x$n_iter}",
      "No. Lik. Calls" = "{x$n_call}",
      "Log. Evidence (\U00B1 Err.)" = "{log_z} (\U00B1 {log_z_sd})"
    ))
  })
}

#' @export
print.summary.ernest_run <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}
