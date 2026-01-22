#' Create a new ernest run object
#'
#' @param x An `ernest_sampler` or `ernest_results` object used to produce a run
#'
#' @param results The list output from nested_sampling_impl.
#'
#' @returns A new ernest_run object (documented in generate)
#' @noRd
new_ernest_run <- function(x, results) {
  UseMethod("new_ernest_run")
}

#' @export
#' @noRd
new_ernest_run.ernest_sampler <- function(x, results) {
  parsed <- parse_results(results)
  new_ernest_run_(x, parsed)
}

#' @export
#' @noRd
new_ernest_run.ernest_run <- function(x, results) {
  prev_iter <- x$n_iter
  old_idx <- vctrs::vec_as_location(
    seq(prev_iter),
    vctrs::vec_size(x$log_lik)
  )
  parsed <- parse_results(results)

  parsed$unit <- rbind(x$samples_unit[old_idx, ], parsed$unit)
  parsed$log_lik <- vctrs::vec_c(x$log_lik[old_idx], parsed$log_lik)
  parsed$id <- vctrs::vec_c(x$id[old_idx], parsed$id)
  parsed$evals <- vctrs::vec_c(x$evals[old_idx], parsed$evals)
  parsed$birth_lik <- vctrs::vec_c(x$birth_lik[old_idx], parsed$birth_lik)
  parsed$n_iter <- x$n_iter + parsed$n_iter
  new_ernest_run_(x, parsed)
}

#' Form the new_ernest_run from samples from the current and previous runs
#'
#' Combines parsed results and live points to construct a new `ernest_run`
#' object.
#'
#' @param x The `ernest_run` or `ernest_sampler` object.
#' @param parsed A list with the previous dead points from the run.
#'
#' @return A new `ernest_run` object.
#' @noRd
new_ernest_run_ <- function(x, parsed) {
  live_order <- order(x$run_env$log_lik)
  samples_unit <- rbind(parsed$unit, x$run_env$unit[live_order, ])
  colnames(samples_unit) <- x$prior$names
  samples <- t(apply(samples_unit, 1, x$prior$fn))
  colnames(samples) <- x$prior$names

  live <- list(
    "log_lik" = x$run_env$log_lik[live_order],
    "id" = live_order,
    "birth_lik" = x$run_env$birth_lik[live_order]
  )
  all_samples <- bind_dead_live(parsed, live, x$n_points, parsed$n_iter)

  log_vol <- drop(get_logvol(x$n_points, n_iter = parsed$n_iter))
  integration <- compute_integral(all_samples$log_lik, log_vol)

  result_elem <- list2(
    "n_iter" = parsed$n_iter,
    "neval" = sum(all_samples$evals),
    !!!integration,
    "id" = all_samples$id,
    "points" = all_samples$points,
    "evals" = all_samples$evals,
    "birth_lik" = all_samples$birth_lik,
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
    run_env = x$run_env,
    seed = attr(x, "seed")
  )

  obj <- do.call(
    new_ernest_sampler,
    list2(!!!sampler_elem, !!!result_elem, .class = "ernest_run")
  )
  # Clear caches
  env_unbind(obj$run_env, env_names(obj$run_env))
  obj
}

#' @srrstats {BS6.0} Default print for return object.
#' @noRd
#' @export
print.ernest_run <- function(x, ...) {
  cli::cli_text("Nested sampling run:")
  cli::cli_bullets(c(
    "* Live points: {x$n_points}",
    "* Sampling method: {format(x$lrps, ...)}",
    "* Prior: {format(x$prior, ...)}"
  ))
  cli::cli_rule(left = "Results")
  log_z <- pretty_round(tail(x$log_evidence, 1), 4)
  log_z_sd <- pretty_round(sqrt(tail(x$log_evidence_var, 1)), 4)
  cli::cli_bullets(c(
    "* Iterations: {x$n_iter}",
    "* Likelihood evaluations: {x$n_evals}",
    "* Log. Volume: {pretty_round(tail(x$log_volume, 1L), 4)}",
    "* Log. Evidence: {log_z} (\U00B1 {log_z_sd})"
  ))
  invisible(x)
}

#' Summarize a nested sampling run
#'
#' Provides a summary of an `ernest_run` object, including key statistics and a
#' tibble of results for each iteration.
#'
#' @param object An `ernest_run` object.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' A list of class `summary.ernest_run` with the following components:
#'
#' * `n_iter`: Integer. Number of iterations performed.
#' * `n_points`: Integer. Number of live points used in the run.
#' * `neval`: Integer. Total number of likelihood function calls.
#' * `log_volume`: Double. Final estimated log-prior volume.
#' * `log_evidence`: Double. Final log-evidence estimate.
#' * `log_evidence_err`: Double. Standard deviation of the log-evidence
#' estimate.
#' * `draws`: Posterior draws as returned by [as_draws()].
#' * `run` A [tibble::tibble].
#'
#' `run` stores the state of the run at each iteration with these columns:
#' * `call`: Cumulative number of likelihood calls.
#' * `log_lik`: Log-likelihood for each sample.
#' * `log_volume`: Estimated log-prior volume.
#' * `log_weight`: Unnormalized log-weights (relative to evidence).
#' * `log_evidence`: Cumulative log-evidence.
#' * `log_evidence_err`: Standard deviation of log-evidence.
#' * `information`: Estimated KL divergence at each iteration.
#'
#' @seealso
#' * [generate()] for details on the `ernest_run` object.
#' * [as_draws()] for more information on `draws` objects.
#'
#' @srrstats {BS6.4} Summary method for results object.
#'
#' @export
#' @examples
#' # Load an example run
#' data(example_run)
#'
#' # Summarize the run and view a tibble of its results.
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
      "neval" = object$neval,
      "log_volume" = tail(sum_df$log_volume, 1),
      "log_evidence" = tail(sum_df$log_evidence, 1L),
      "log_evidence_err" = tail(sum_df$log_evidence_err, 1L),
      "run" = sum_df,
      "draws" = as_draws(object)
    ),
    class = "summary.ernest_run"
  )
}

#' @noRd
#' @export
format.summary.ernest_run <- function(x, ...) {
  log_z <- pretty_round(x$log_evidence, 4)
  log_z_sd <- pretty_round(x$log_evidence_err, 4)
  glue::glue(
    "No. Points: {x$n_points}",
    "No. Iterations: {x$n_iter}",
    "No. Calls: {x$neval}",
    "Log. Volume: {pretty_round(x$log_volume, 4)}",
    "Log. Evidence: {log_z} (\U00B1 {log_z_sd})",
    .sep = "\n"
  )
}

#' @noRd
#' @export
print.summary.ernest_run <- function(x, ...) {
  cli::cli_text("nested sampling result summary {.cls {class(x)}}")
  lines <- strsplit(format(x), split = "\n")[[1]]
  names(lines) <- rep("*", length(lines))
  cli::cli_bullets(lines[1:2])
  cli::cli_rule()
  cli::cli_bullets(lines[3:5])
  invisible(x)
}

# HELPERS FOR ERNEST_RUN-----

#' Parse the results from nested_sampling_impl into a list
#'
#' Converts the output from `nested_sampling_impl` into a structured list of
#' vectors.
#'
#' @param results Output from `nested_sampling_impl`, which is a list that
#' contains these elements:
#' * `dead_unit`: Sample points in the unit hypercube.
#' * `dead_log_lik`: Log-likelihoods of dead points.
#' * `dead_id`: IDs of dead points.
#' * `dead_evals`: Number of likelihood evals for each dead point.
#' * `dead_birth`: The log-likelihood of the point used to create each dead
#' point.
#'
#' @return A named list of vectors and the number of iterations.
#' @noRd
parse_results <- function(results) {
  dead_unit <- do.call(rbind, results$dead_unit)
  dead_log_lik <- list_c(results$dead_log_lik)
  dead_id <- list_c(results$dead_id)
  dead_evals <- list_c(results$dead_evals)
  dead_birth <- list_c(results$dead_birth)
  n_iter <- vctrs::vec_size(dead_log_lik)
  list(
    "unit" = dead_unit,
    "log_lik" = dead_log_lik,
    "id" = dead_id,
    "evals" = dead_evals,
    "birth_lik" = dead_birth,
    "n_iter" = n_iter
  )
}

#' Merge dead and live samples together
#'
#' Combines dead and live sample information into a single data frame list.
#'
#' @param dead The list object from `parse_results`.
#' @param live The log-likelihood, id, and birth_lik vectors from the current live
#' points.
#' @param n_iter Number of iterations used for the run.
#' @param n_points Number of live points used for the run.
#'
#' @return A data frame list of vectors, all of length `n_points + n_iter`.
#' @noRd
bind_dead_live <- function(dead, live, n_points, n_iter) {
  vctrs::df_list(
    "log_lik" = vctrs::vec_c(dead$log_lik, live$log_lik, .ptype = double()),
    "id" = vctrs::vec_c(dead$id, live$id, .ptype = integer()),
    # TODO points should be collected as a list of int during the run.
    "points" = vctrs::vec_c(
      rep(n_points, n_iter),
      seq(n_points, 1),
      .ptype = integer()
    ),
    "evals" = vctrs::vec_c(dead$evals, rep(0L, n_points), .ptype = integer()),
    "birth_lik" = vctrs::vec_c(
      dead$birth_lik,
      live$birth_lik,
      .ptype = double()
    )
  )
}
