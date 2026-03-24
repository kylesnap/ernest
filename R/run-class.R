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
  parsed <- parse_results(results, x$live_env)
  new_ernest_run_(x, parsed)
}

#' @export
#' @noRd
new_ernest_run.ernest_run <- function(x, results) {
  prev_run <- butcher_run(x, keep_live = FALSE)
  parsed <- parse_results(results, x$live_env)

  parsed$dead <- vctrs::vec_rbind(
    vctrs::data_frame(!!!prev_run),
    vctrs::data_frame(!!!parsed$dead)
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
  all_samples <- bind_dead_live(parsed, x$nlive)
  samples_unit <- all_samples$unit
  colnames(samples_unit) <- x$prior$names
  samples <- t(apply(samples_unit, 1, x$prior$fn)) # TODO: Try vectorized?
  colnames(samples) <- x$prior$names

  integration <- compute_integral(
    all_samples$log_lik,
    get_log_vol(x$nlive, niter = parsed$niter)
  )

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
      "id" = as.integer(all_samples$id),
      "evaluations" = as.integer(all_samples$evals),
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
    live_env = x$live_env,
    seed = attr(x, "seed")
  )

  obj <- do.call(
    new_ernest_sampler,
    list2(!!!sampler_elem, !!!result_elem, .class = "ernest_run")
  )
  env_unbind(obj$live_env, env_names(obj$live_env))
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

# HELPERS FOR ERNEST_RUN-----

#' Parse the results from nested_sampling_impl into a list
#'
#' Converts the output from `nested_sampling_impl` into a structured list of
#' vectors.
#'
#' @param results Output from `nested_sampling_impl`.
#' @param live_env The live environment from the run, used to extract live
#' points.
#'
#' @return A named list of vectors summarizing information from the dead
#' and live points.
#' @noRd
parse_results <- function(results, live_env) {
  dead <- vctrs::df_list(
    "unit" = results$dead_unit,
    "log_lik" = results$dead_log_lik,
    "id" = results$dead_id,
    "evals" = results$dead_evals,
    "birth_lik" = results$dead_birth
  )
  live_order <- order(live_env$log_lik)
  live <- vctrs::df_list(
    "unit" = live_env$unit[live_order, , drop = FALSE],
    "log_lik" = live_env$log_lik[live_order],
    "id" = seq_len(nrow(live_env$unit))[live_order],
    "evals" = 0L,
    "birth_lik" = live_env$birth_lik[live_order]
  )
  list("dead" = dead, "live" = live, "niter" = length(dead$log_lik))
}

#' Simplify nested sampling results into a basic list for merging.
#'
#' @param x An ernest_run object.
#' @param keep_live Whether to include the live set in the output.
#'
#' @returns A list containing a subset of the elements from `x`.
#' @noRd
butcher_run <- function(x, keep_live = TRUE) {
  run <- vctrs::df_list(
    "unit" = x$samples$unit_cube,
    "log_lik" = x$weights$log_lik,
    "id" = x$weights$id,
    "evals" = x$weights$evaluations,
    "birth_lik" = x$weights$birth_lik
  )
  if (keep_live) {
    return(run)
  }
  dead <- vctrs::vec_as_location(run$evals != 0L, length(run$evals))
  vctrs::df_list(
    "unit" = run$unit[dead, , drop = FALSE],
    "log_lik" = run$log_lik[dead],
    "id" = run$id[dead],
    "evals" = run$evals[dead],
    "birth_lik" = run$birth_lik[dead],
  )
}

#' Merge dead and live samples together
#'
#' Combines dead and live sample information into a single data frame list.
#'
#' @param dead The list object from `parse_results`.
#' @param live The log-likelihood, id, and birth_lik vectors from the
#' current live set.
#'
#' @return A data frame list of vectors, all of length `nlive + niter`.
#' @noRd
bind_dead_live <- function(parsed, nlive, call = caller_env()) {
  all_samples <- vctrs::df_list(
    !!!vctrs::vec_rbind(
      vctrs::data_frame(!!!parsed$dead),
      vctrs::data_frame(!!!parsed$live)
    )
  )
  if (!vctrs::list_all_size(all_samples, size = nlive + parsed$niter)) {
    cli::cli_abort(
      "Sampler contains an unexpected number of samples.",
      "!" = "Expected {nlive + parsed$niter}",
      "!" = "Observed: {nrow(all_samples)}",
      "i" = "This likely indicates a problem internal to {.pkg ernest}.",
      call = call
    )
  }
  if (is.unsorted(all_samples$log_lik)) {
    cli::cli_warn(
      "`log_lik` values in the sampler are not in ascending order.",
      "i" = "This likely indicates a problem internal to {.pkg ernest}.",
      call = call
    )
  }
  all_samples
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
