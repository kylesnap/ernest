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
  new_ernest_run_(x, results)
}

#' @export
#' @noRd
new_ernest_run.ernest_run <- function(x, results) {
  prev_run <- as_ernest_rcrd(x, keep_live = FALSE)
  new_ernest_run_(x, vctrs::vec_c(prev_run, results))
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
  all_samples <- c(parsed, extract_live_points(x$live_env))
  niter <- sum(field(all_samples, "evals") != 0L)
  integration <- compute_integral(field(all_samples, "log_lik"), x$nlive)
  unit <- as.list(all_samples)[["unit"]]
  colnames(unit) <- x$prior$names
  original <- t(apply(unit, 1, x$prior$fn))
  colnames(original) <- x$prior$names

  result_elem <- list(
    "niter" = niter,
    "neval" = sum(field(all_samples, "evals")),
    "log_evidence" = tail(integration$log_evidence, 1L),
    "log_evidence_err" = sqrt(tail(integration$log_evidence_var, 1L)),
    "information" = tail(integration$information, 1L),
    "samples" = list(
      "original" = original,
      "unit_cube" = unit
    ),
    "weights" = vctrs::df_list(
      "id" = field(all_samples, "id"),
      "evaluations" = as.integer(field(all_samples, "evals")),
      "log_lik" = integration$log_lik,
      "log_weight" = integration$log_w,
      "imp_weight" = exp(
        integration$log_w - tail(integration$log_evidence, 1L)
      ),
      "birth_lik" = field(all_samples, "birth_lik")
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
