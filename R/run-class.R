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
  prev_iter <- x$niter
  old_idx <- vctrs::vec_as_location(
    seq(prev_iter),
    vctrs::vec_size(x$weights$log_lik)
  )
  parsed <- parse_results(results)

  parsed$unit <- rbind(x$samples$unit_cube[old_idx, ], parsed$unit)
  parsed$log_lik <- vctrs::vec_c(x$weights$log_lik[old_idx], parsed$log_lik)
  parsed$id <- vctrs::vec_c(x$weights$id[old_idx], parsed$id)
  parsed$evals <- vctrs::vec_c(x$weights$evaluations[old_idx], parsed$evals)
  parsed$birth_lik <- vctrs::vec_c(
    x$weights$birth_lik[old_idx],
    parsed$birth_lik
  )
  parsed$niter <- x$niter + parsed$niter
  new_ernest_run_(x, parsed)
}

#' Form the new_ernest_run from samples from the current and previous runs
#'
#' Combines parsed results and the live set to construct a new `ernest_run`
#' object.
#'
#' @param x The `ernest_run` or `ernest_sampler` object.
#' @param parsed A list with the previous dead points from the run.
#'
#' @return The object described by generate.
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
  all_samples <- bind_dead_live(parsed, live, x$nlive, parsed$niter)
  integration <- compute_integral(all_samples$log_lik, parsed$niter, x$nlive)

  result_elem <- list2(
    "niter" = parsed$niter,
    "neval" = sum(all_samples$evals),
    "log_evidence" = tail(integration$log_evidence, 1L),
    "log_evidence_err" = sqrt(tail(integration$log_evidence_var, 1L)),
    "information" = tail(integration$information, 1L),
    "samples" = list(
      "original" = samples,
      "unit_cube" = samples_unit
    ),
    "weights" = vctrs::df_list(
      "id" = all_samples$id,
      "evaluations" = all_samples$evals,
      "log_lik" = all_samples$log_lik,
      "log_weight" = integration$log_w,
      "imp_weight" = exp(
        integration$log_w - tail(integration$log_evidence, 1L)
      ),
      "birth_lik" = all_samples$birth_lik
    )
  )

  sampler_elem <- list(
    log_lik_fn = x$log_lik_fn,
    prior = x$prior,
    lrps = x$lrps,
    nlive = x$nlive,
    first_update = x$first_update,
    update_interval = x$update_interval,
    run_env = x$run_env,
    seed = attr(x, "seed")
  )

  obj <- do.call(
    new_ernest_sampler,
    list2(!!!sampler_elem, !!!result_elem, .class = "ernest_run")
  )
  env_unbind(obj$run_env, env_names(obj$run_env))
  obj
}

#' @srrstats {BS6.0} Default print for return object.
#' @importFrom prettyunits pretty_round
#' @noRd
#' @export
print.ernest_run <- function(x, ...) {
  cli::cli_text("Nested sampling run:")
  cli::cli_bullets(c(
    "* No. points: {x$nlive}",
    "* Sampling method: {format(x$lrps, ...)}",
    "* Prior: {format(x$prior, ...)}"
  ))
  cli::cli_rule(left = "Results")
  log_z <- pretty_round(x$log_evidence, 4)
  log_z_sd <- pretty_round(x$log_evidence_err, 4)
  h <- prettyunits::pretty_signif(x$information, 4)
  cli::cli_bullets(c(
    "* Iterations: {x$niter}",
    "* Likelihood evals.: {x$neval}",
    "* Log-evidence: {log_z} (\U00B1 {log_z_sd})",
    "* Information: {h}"
  ))
  invisible(x)
}

#' Summarize a nested sampling run
#'
#' Returns a concise summary of an `ernest_run` object, including key
#' statistics and a description of the posterior distribution.
#'
#' @param object [[ernest_run]]\cr Results from a nested sampling run.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns `[summary.ernest_run]`
#' A named list, containing:
#' * `nlive`: `[integer(1)]` Number of points in the live set.
#' * `niter`: `[integer(1)]` Number of iterations.
#' * `neval`: `[integer(1)]` Number of likelihood evaluations.
#' * `log_evidence`: `[numeric(1)]` Log-evidence estimate.
#' * `log_evidence_err`: `[numeric(1)]` Standard error of log-evidence.
#' * `information`: `[numeric(1)]` Estimated Kullback-Leibler divergence between
#' the prior and posterior.
#' * `reweighted_samples`: [[posterior::draws_matrix]] Posterior samples,
#' resampled by normalized weights.
#' * `mle`: `[list]` Maximum likelihood estimate extracted during the run,
#' stored in a list with the elements:
#'    * `log_lik`: `[double(1)]` The maximum log-likelihood value.
#'    * `original`, `unit_cube`: `[double(n_dim)]` The parameter values at the
#'    MLE, expressed in the original parameter space and within the unit cube.
#' * `posterior`: [[tibble]] with columns for the posterior mean, sd, median,
#' and the 15th and 85th percentiles for each parameter.
#' * `seed`: The RNG seed used.
#'
#' @seealso
#' * [generate()] for details on the `ernest_run` object.
#' * [as_draws()] for details on how posterior samples are extracted.
#'
#' @srrstats {BS6.4} Summary method for results object.
#'
#' @examples
#' data(example_run)
#' run_sm <- summary(example_run)
#' run_sm
#' run_sm$posterior
#' @export
summary.ernest_run <- function(object, ...) {
  check_dots_empty()
  nlive <- object$nlive
  seed <- attr(object, "seed")
  niter <- object$niter
  neval <- object$neval
  log_evidence <- object$log_evidence
  log_evidence_err <- object$log_evidence_err
  information <- object$information

  # Posterior samples and weights
  draws <- as_draws(object)
  weights <- object$weights$imp_weight
  norm_weights <- exp(weights - max(weights))
  norm_weights <- norm_weights / sum(norm_weights)

  # Resampled posterior samples
  reweighted_samples <- posterior::resample_draws(draws)

  # MLE
  idx_mle <- which.max(object$weights$log_lik)
  mle <- list(
    log_lik = object$weights$log_lik[idx_mle],
    "original" = object$samples$original[idx_mle, ],
    "unit_cube" = object$samples$unit_cube[idx_mle, ]
  )

  # Posterior summary statistics
  draws_matrix <- posterior::as_draws_matrix(draws)
  posterior <- posterior::summarise_draws(
    draws_matrix,
    "mean",
    "sd",
    "median",
    \(x) {
      y <- stats::quantile(x, probs = c(0.15, 0.85))
      names(y) <- c("q15", "q85")
      y
    }
  )

  structure(
    list(
      nlive = nlive,
      niter = niter,
      neval = neval,
      log_evidence = log_evidence,
      log_evidence_err = log_evidence_err,
      information = information,
      reweighted_samples = reweighted_samples,
      mle = mle,
      posterior = posterior,
      seed = seed
    ),
    class = "summary.ernest_run"
  )
}

#' @noRd
#' @export
print.summary.ernest_run <- function(x, ...) {
  cli::cli_text("Summary of nested sampling run:")
  cli::cli_rule(left = "Run Information")
  log_z <- pretty_round(x$log_evidence, 4)
  log_z_sd <- pretty_round(x$log_evidence_err, 4)
  cli::cli_bullets(c(
    "* No. points: {x$nlive}",
    "* Iterations: {x$niter}",
    "* Likelihood evals.: {x$neval}",
    "* Log-evidence: {log_z} (\U00B1 {log_z_sd})",
    "* Information: {prettyunits::pretty_signif(x$information, 4)}"
  ))
  if (!is.na(x$seed)) {
    cli::cli_bullets(c("* RNG seed: {x$seed}"))
  }

  cli::cli_rule(left = "Posterior Summary")
  posterior <- x$posterior
  n_show <- min(6, nrow(posterior))
  print(posterior[seq_len(n_show), ])
  if (n_show < nrow(posterior)) {
    cli::cli_text("Use {.code x$posterior} to see the full posterior summary.")
  }

  cli::cli_rule(left = "Maximum Likelihood Estimate (MLE)")
  cli::cli_bullets(c(
    "* Log-likelihood: {pretty_round(x$mle$log_lik, 4)}",
    "* Original parameters: {pretty_round(x$mle$original, 4)}"
  ))
  invisible(x)
}

# HELPERS FOR ERNEST_RUN-----

#' Parse the results from nested_sampling_impl into a list
#'
#' Converts the output from `nested_sampling_impl` into a structured list of
#' vectors.
#'
#' @param results Output from `nested_sampling_impl`.
#'
#' @return A named list of vectors and the number of iterations.
#' @noRd
parse_results <- function(results) {
  dead_unit <- do.call(rbind, results$dead_unit)
  dead_log_lik <- list_c(results$dead_log_lik)
  dead_id <- list_c(results$dead_id)
  dead_evals <- list_c(results$dead_evals)
  dead_birth <- list_c(results$dead_birth)
  niter <- vctrs::vec_size(dead_log_lik)
  list(
    "unit" = dead_unit,
    "log_lik" = dead_log_lik,
    "id" = dead_id,
    "evals" = dead_evals,
    "birth_lik" = dead_birth,
    "niter" = niter
  )
}

#' Merge dead and live samples together
#'
#' Combines dead and live sample information into a single data frame list.
#'
#' @param dead The list object from `parse_results`.
#' @param live The log-likelihood, id, and birth_lik vectors from the current
#' live set.
#' @param niter Number of iterations used for the run.
#' @param nlive Number of points in the live set.
#'
#' @return A data frame list of vectors, all of length `nlive + niter`.
#' @noRd
bind_dead_live <- function(dead, live, nlive, niter) {
  vctrs::df_list(
    "log_lik" = vctrs::vec_c(dead$log_lik, live$log_lik, .ptype = double()),
    "id" = vctrs::vec_c(dead$id, live$id, .ptype = integer()),
    "points" = vctrs::vec_c(
      rep(nlive, niter),
      seq(nlive, 1),
      .ptype = integer()
    ),
    "evals" = vctrs::vec_c(dead$evals, rep(0L, nlive), .ptype = integer()),
    "birth_lik" = vctrs::vec_c(
      dead$birth_lik,
      live$birth_lik,
      .ptype = double()
    )
  )
}
