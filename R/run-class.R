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
#' @slot spec A pairlist, describing the `log_lik`, `prior`, `sampler`,
#' `first_update`, `update_interval` used to produce this run, as well as
#' the `min_logz` value the run was terminated on.
#'
#' @srrstats {BS5.0, BS5.1, BS5.2, BS5.3} ernest_run contains the prior
#' specification, log. lik function, and the est. remaining log evidence within
#' the `spec` pairlist. The random seed is also stored as an attribute.
#'
#' @details
#' The `ernest_run` object is returned by running a nested sampling procedure
#' in the `ernest` package. It can be used for posterior analysis, evidence
#' estimation, and diagnostic plotting.
#'
#' @seealso  [generate()] for how
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
  dead_birth,
  remaining_logz
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
  args <- pairlist2(
    log_lik = private$log_lik,
    prior = private$prior,
    sampler = private$lrps$as_string(),
    first_update = private$first_update,
    update_interval = private$update_interval,
    "remaining_log_evidence" = remaining_logz
  )

  new_ernest_run(res, n_live, n_dead, live_order, args, private$seed)
}

new_ernest_run <- function(res, n_live, n_dead, live_order, args, seed) {
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
      "samples_unit" = res$samples_unit,
      "spec" = args
    ),
    "live_loc" = live_loc,
    "seed" = seed,
    class = "ernest_run"
  )
}

#' @export
format.ernest_run <- function(x, ...) {
  smry <- summary(x)
  log_z <- smry$log_evidence
  log_z_sd <- smry$log_evidence_err
  cli::cli_format_method({
    cli::cli_div(theme = list(.val = list(digits = 3)))
    first_line <- sprintf(
      "%s %s",
      "An {.cls ernest_run}:",
      "{x$n_points} points x {x$n_iter} iter x {x$n_calls} lik. calls"
    )
    cli::cli_bullets(c(
      first_line,
      ">" = "Log. Evidence: {.val {log_z}} \U00B1 {.val {log_z_sd}}"
    ))
  })
}

#' @srrstats {BS6.0} Default print for results objects.
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
#' * `log_volume`, `log_evidence`, `log_evidence_err`: The final estimates of
#' the quantities performed by the run that generated `object`.
#' * `run`, A tibble with `n_iter + n_points` rows, containing the vectors
#'  `call`, `log_lik`, `log_volume`, `log_weight`, `log_evidence`,
#'  `log_evidence_err`, and `information`.
#'
#' @seealso [ernest_run-class] for the `ernest_run` object. [as_draws()] for
#' how to summarize the posterior distribution generated by nested sampling.
#' @srrstats {BS6.4} Summary method for ernest_run.
#' @export
#' @examples
#' # Load an example run
#' ernest_run_example <- run_example()
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
  })
}

#' @export
print.summary.ernest_run <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
