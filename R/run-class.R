#' Create a new ernest run object
#' @param x An `ernest_sampler` or `ernest_results` object used to produce a run
#' @param results The list output from nested_sampling_impl.
#' @returns A new ernest_run object.
#' @noRd
new_ernest_run <- function(x, results) {
  UseMethod("new_ernest_run")
}

#' @method new_ernest_run ernest_sampler
#' @export
new_ernest_run.ernest_sampler <- function(x, results) {
  parsed <- parse_results(results)
  new_ernest_run_(x, parsed)
}

#' @method new_ernest_run ernest_run
#' @export
new_ernest_run.ernest_run <- function(x, results) {
  prev_iter <- x$n_iter
  old_idx <- vctrs::vec_as_location(
    seq(prev_iter),
    vctrs::vec_size(x$log_lik)
  )
  parsed <- parse_results(results)

  parsed$unit <- rbind(x$samples_unit[old_idx, ], parsed$unit)
  parsed$log_lik <- vec_c(x$log_lik[old_idx], parsed$log_lik)
  parsed$id <- vec_c(x$id[old_idx], parsed$id)
  parsed$calls <- vec_c(x$calls[old_idx], parsed$calls)
  parsed$birth <- vec_c(x$birth[old_idx], parsed$birth)
  parsed$n_iter <- x$n_iter + parsed$n_iter
  new_ernest_run_(x, parsed)
}

#' Form the new_ernest_run from samples from the current and previous runs
#' @param x The ernest_run or ernest_sampler object.
#' @param parsed A list with the previous dead points from the run.
#' @returns A new ernest_run object.
#' @noRd
new_ernest_run_ <- function(x, parsed) {
  live_order <- order(x$live_points$log_lik)
  samples_unit <- rbind(parsed$unit, x$live_points$unit[live_order, ])
  colnames(samples_unit) <- x$prior$varnames
  samples <- t(apply(samples_unit, 1, x$prior$fn))
  colnames(samples) <- x$prior$varnames

  live <- list(
    "log_lik" = x$live_points$log_lik[live_order],
    "id" = live_order,
    "birth" = x$live_points$birth[live_order]
  )
  all_samples <- bind_dead_live(parsed, live, x$n_points, parsed$n_iter)

  log_vol <- cumsum(-1 * (all_samples$points**-1))
  integration <- compute_integral(all_samples$log_lik, log_vol)

  result_elem <- list2(
    "n_iter" = parsed$n_iter,
    "n_calls" = sum(all_samples$calls),
    !!!integration,
    "id" = all_samples$id,
    "points" = all_samples$points,
    "calls" = all_samples$calls,
    "birth" = all_samples$birth,
    "samples" = samples,
    "samples_unit" = samples_unit
  )

  sampler_elem <- list(
    log_lik_fn = x$log_lik_fn,
    prior = x$prior,
    lrps = x$lrps,
    n_points = x$n_points,
    first_update = x$first_update,
    update_interval = x$update_interval,
    live_points = x$live_points
  )

  do.call(
    new_ernest_sampler,
    list2(!!!sampler_elem, !!!result_elem, .class = "ernest_run")
  )
}

#' Format method for ernest_run
#' @param x The ernest_run object.
#' @return A formatted string.
#' @noRd
#' @export
format.ernest_run <- function(x, ...) {
  log_z <- tail(x$log_evidence, 1)
  log_z_sd <- sqrt(tail(x$log_evidence_var, 1))
  cli::cli_format_method({
    cli::cli_div(theme = list(.val = list(digits = 3)))
    cli::cli_h3("Nested sampling run:")
    cli::cli_text("No. Iterations: {x$n_iter}")
    cli::cli_text("No. Calls: {x$n_calls}")
    cli::cli_text("Log. Evidence: {.val {log_z}} (\U00B1 {.val {log_z_sd}})")
  })
}

#' Print method for ernest_run
#' @param x The ernest_run object.
#' @return Itself, invisibly.
#' @srrstats {BS6.0} Default print for results objects.
#' @noRd
#' @export
print.ernest_run <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Summarise a nested sampling run
#'
#' Provides a summary of an `ernest_run` object, including key statistics and a
#' tibble of results for each iteration.
#'
#' @param object An object of class `ernest_run`.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' A list of class `summary.ernest_run` with the following components:
#'
#' * `n_iter`: Integer. Number of iterations performed.
#' * `n_points`: Integer. Number of live points used in the run.
#' * `n_calls`: Integer. Total number of likelihood function calls.
#' * `log_volume`: Double. Final estimated log-prior volume.
#' * `log_evidence`: Double. Final log-evidence estimate.
#' * `log_evidence_err`: Double. Standard deviation of the log-evidence
#' estimate.
#' * `draws`: Posterior draws as returned by [as_draws()].
#'
#' In addition, the `run` tibble stores the state of the run at each iteration
#' within these columns:
#' * `call`: Cumulative number of likelihood calls.
#' * `log_lik`: Log-likelihood for each sample.
#' * `log_volume`: Estimated log-prior volume.
#' * `log_weight`: Unnormalized log-weights (relative to evidence).
#' * `log_evidence`: Cumulative log-evidence.
#' * `log_evidence_err`: Standard deviation of log-evidence.
#' * `information`: Estimated KL divergence at each iteration.
#'
#' @seealso
#' * [generate()] describes the `ernest_run` object.
#' * [as_draws()] provides more information on `draws` objects.
#' @srrstats {BS6.4} Summary method for results object.
#' @export
#' @examples
#' # Load an example run
#' data(example_run)
#'
#' # Summarise the run and view a tibble of its results.
#' run_sm <- summary(example_run)
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

#' Format method for ernest_run summary.
#' @param x The summary.ernest_run object.
#' @return A formatted string.
#' @noRd
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

#' Format method for ernest_run summary.
#' @param x The summary.ernest_run object.
#' @return x, invisibly.
#' @noRd
#' @export
print.summary.ernest_run <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# HELPERS FOR ERNEST_RUN-----

#' Parse the results from nested_sampling_impl into a list
#' @param results Output from nested_sampling_impl
#' @return A named list of vectors and the number of iterations.
#' @noRd
parse_results <- function(results) {
  dead_unit <- do.call(rbind, results$dead_unit)
  dead_log_lik <- list_c(results$dead_log_lik)
  dead_id <- list_c(results$dead_id)
  dead_calls <- list_c(results$dead_calls)
  dead_birth <- list_c(results$dead_birth)
  n_iter <- vctrs::vec_size(dead_log_lik)
  list(
    "unit" = dead_unit,
    "log_lik" = dead_log_lik,
    "id" = dead_id,
    "calls" = dead_calls,
    "birth" = dead_birth,
    "n_iter" = n_iter
  )
}

#' Merge dead and live samples together
#' @param dead The list object from parse_results.
#' @param live The log_lik, id, and birth vectors from the current live points.
#' @param n_iter,n_points The iterations and live points used for the run.
#' @returns A df_list of vectors, that are all of the length n_points + n_iter.
#' @importFrom vctrs vec_c
#' @noRd
bind_dead_live <- function(dead, live, n_points, n_iter) {
  vctrs::df_list(
    "log_lik" = vec_c(dead$log_lik, live$log_lik, .ptype = double()),
    "id" = vec_c(dead$id, live$id, .ptype = integer()),
    # TODO points should be collected as a list of int during the run.
    "points" = vec_c(
      rep(n_points, n_iter),
      seq(n_points, 1),
      .ptype = integer()
    ),
    "calls" = vec_c(dead$calls, rep(0L, n_points), .ptype = integer()),
    "birth" = vec_c(dead$birth, live$birth, .ptype = integer())
  )
}
