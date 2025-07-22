#' Results from nested sampling runs
#'
#' The `ernest_run` object contains the results of a nested sampling run. It has
#' methods for summary, plotting, and for simulating the uncertainty around the
#' run's estimates.
#'
#' @name ernest_run-class
#' @aliases ernest_run
#' @docType class
#'
#' @slot n_iter Total number of iterations performed.
#' @slot n_points Number of points drawn into the live set.
#' @slot n_calls Number of calls to the log likelihood function.
#' @slot log_lik A vector of log likelihood values associated with each point
#' generated during the run.
#' @slot log_volume A vector of the estimated prior volumes associated with the
#' removal of each point from the live set.
#' @slot log_weight A vector of unnormalized posterior weights for each point.
#' @slot log_evidence A vector of log. evidence estimates, generated after the
#' removal of each point.
#' @slot log_evidence_var A vector of uncertainty values associated with each
#' entry in `log_evidence`. 1.
#' @slot information A vector of the estimated KL-divergence (or information)
#' between the prior and posterior distributions.
#' @slot id The index of each point within the live set.
#' @slot points The number of live points associated with each point's removal.
#' @slot calls The number of likelihood calls made when generating a replacement
#' live point.
#' @slot birth The iteration at which the point was created and added to the
#' live set.
#' @slot samples A matrix of the sampled points, expressed in the units of the
#' prior space.
#' @slot samples_unit Identical to `samples`, but expressed in the units of the
#' 0-1 hypercube.
#'
#' @srrstats {BS5.0} Return values should include starting value(s) or seed(s),
#' including values for each sequence where multiple sequences are included.
#'
#' @details
#' The `ernest_run` object is returned by running a nested sampling procedure
#' in the `ernest` package. It can be used for posterior analysis, evidence
#' estimation, and diagnostic plotting.
#'
#' @seealso [ernest_run_example] for an example object, and [generate()] for how
#' to create a new `ernest_run` object. [plot()] and [calculate()] on how to use
#' `ernest_run` objects to examine uncertainty in the log volume estimates.
NULL

#' Internal method for constructing the ernest_run object
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

  new_ernest_run(res, n_live, n_dead, live_order, private$seed)
}

new_ernest_run <- function(res, n_live, n_dead, live_order, seed) {
  log_vol <- cumsum(-1 * (res$points**-1))
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
    "seed" = seed,
    class = "ernest_run"
  )
}

#' @export
format.ernest_run <- function(x, ...) {
  smry <- summary(x)
  cli::cli_format_method({
    cli::cli_div(theme = list(.val = list(digits = 3)))
    cli::cli_bullets(c(
      "An {.cls ernest_run}: {x$n_points} points x {x$n_iter} iter x {x$n_calls} lik. calls",
      ">" = "Log. Evidence: {.val {smry$log_evidence}} \U00B1 {.val {smry$log_evidence_err}}"
    ))
  })
}

#' @export
print.ernest_run <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Summarise a nested sampling run
#'
#' Provides a summary of an `ernest_run` object.
#'
#' @param object (ernest_run) An object of class `ernest_run`.
#' @inheritParams rlang::args_dots_empty
#'
#' @return An object of class `summary.ernest_run`, a list with:
#' * `n_iter`: Number of iterations (number of dead points).
#' * `n_points`: Number of live points at the end of the run.
#' * `log_volume`, `log_evidence`, `log_evidence_err`: The final estimates of the
#' quantities performed by the run that generated `object`.
#' * `run`, A tibble with `n_iter + n_points` rows, containing the vectors
#'  `call`, `log_lik`, `log_volume`, `log_weight`, `log_evidence`,
#'  `log_evidence_err`, and `information`.
#'
#' @seealso [ernest_run-class] for the `ernest_run` object. [as_draws()] for
#' how to summarize the posterior distribution generated by nested sampling.
#' @export
#' @examples
#' # Load an example run
#' data(ernest_run_example)
#'
#' # Summarise the run and view a tibble of its results.
#' run_sm <- summary(ernest_run_example)
#' run_sm
#' run_sm$run
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
      "run" = sum_df,
      "draws" = as_draws(object)
    ),
    class = "summary.ernest_run"
  )
}

#' @export
format.summary.ernest_run <- function(x, ...) {
  log_z <- formatC(x$log_evidence, digits = 4, format = "fg")
  log_z_sd <- formatC(x$log_evidence_err, digits = 4, format = "fg")

  cli::cli_format_method({
    cli::cli_h1("Nested Sampling Results from {.cls ernest_run}")
    cli::cli_dl(c(
      "No. Points" = "{x$n_points}",
      "No. Iterations" = "{x$n_iter}",
      "No. Lik. Calls" = "{x$n_call}",
      "Log. Evidence" = "{log_z} (\U00B1 {log_z_sd})"
    ))
    cli::cli_h3("Weighted Posterior Distribution")
    cli::cat_print(posterior::summarise_draws(
      posterior::resample_draws(x$draws),
      posterior::default_summary_measures()
    ))
  })
}

#' @export
print.summary.ernest_run <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
