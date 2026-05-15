#' Create a new ernest run object
#'
#' @param x An `ernest_sampler` object.
#' @param rcrd An `ernest_rcrd` object.
#'
#' @returns A new ernest_run object (documented in generate).
#'
#' @noRd
new_ernest_run <- function(x, rcrd) {
  # all_samples <- vec_c(parsed, extract_live_points(x$live_env))
  check_class(x, "ernest_sampler")
  check_class(rcrd, "ernest_rcrd")
  rcrd_is_run(rcrd)
  if (x$lrps$nvar != attr(rcrd, "nvar")) {
    cli::cli_abort("The number of variables in `x` and `rcrd` do not match.")
  }

  niter <- sum(field(rcrd, "neval") != 0L)
  integration <- compute_integral(rcrd)

  result_elem <- list(
    "niter" = niter,
    "neval" = sum(field(rcrd, "neval")),
    "log_evidence" = tail(integration$log_evidence, 1L),
    "log_evidence_err" = sqrt(tail(integration$log_evidence_var, 1L)),
    "log_weight" = as.double(integration$log_weight),
    "information" = tail(integration$information, 1L),
    "rcrd" = rcrd
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

# #' @export
# #' @noRd
# new_ernest_run.ernest_sampler <- function(x, results) {
#   new_ernest_run_(x, results)
# }

# #' @export
# #' @noRd
# new_ernest_run.ernest_run <- function(x, results) {
#   prev_run <- x$rcrd[vctrs::vec_as_location(
#     field(x$rcrd, "neval") != 0L,
#     length(x$rcrd)
#   )]
#   new_ernest_run_(x, vec_c(prev_run, results))
# }

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

#' @srrstats {BS6.0} Default print for return object.
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
  log_z <- round(x$log_evidence, 4)
  log_z_sd <- round(x$log_evidence_err, 4)
  h <- signif(x$information, 4)
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
#' @returns
#' A named list, containing:
#' * `nlive`: `[integer(1)]` Number of points in the live set.
#' * `niter`: `[integer(1)]` Number of iterations performed.
#' * `neval`: `[integer(1)]` Number of times the likelihood function was
#' evaluated.
#' * `log_evidence`: `[numeric(1)]` Log-evidence estimate.
#' * `log_evidence_err`: `[numeric(1)]` Standard error of log-evidence.
#' * `information`: `[numeric(1)]` Estimated Kullback-Leibler divergence between
#' the prior and posterior.
#' * `reweighted_samples`: [[posterior::draws_matrix]] Posterior samples,
#' resampled by normalized weights.
#' * `mle`: `[list]` Maximum likelihood estimate extracted during the run,
#' stored in a list with the elements:
#'    * `log_lik`: `[double(1)]` The maximum log-likelihood value.
#'    * `original`, `unit_cube`: `[double(nvar)]` The parameter values at the
#'    MLE, expressed in the original parameter space and within the unit cube.
#' * `posterior`: [[data.frame]] with columns for the posterior mean, sd,
#' median, and the 15th and 85th percentiles for each parameter.
#' * `seed`: The RNG seed used.
#'
#' @seealso [generate.ernest_run()] [as_draws.ernest_run()]
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
  weights <- weights(object)
  norm_weights <- exp(weights - max(weights))
  norm_weights <- norm_weights / sum(norm_weights)

  # Resampled posterior samples
  reweighted_samples <- posterior::resample_draws(draws)

  # MLE
  log_lik <- if (!is.null(object$rcrd)) {
    field(object$rcrd, "log_lik")
  } else {
    object$weights$log_lik
  }
  idx_mle <- which.max(log_lik)
  mle <- list(
    log_lik = log_lik[idx_mle],
    "original" = object$prior$fn(field(object$rcrd[[idx_mle]], "unit")),
    "unit_cube" = field(object$rcrd[[idx_mle]], "unit")
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
  log_z <- round(x$log_evidence, 4)
  log_z_sd <- round(x$log_evidence_err, 4)
  cli::cli_bullets(c(
    "* No. points: {x$nlive}",
    "* Iterations: {x$niter}",
    "* Likelihood evals.: {x$neval}",
    "* Log-evidence: {log_z} (\U00B1 {log_z_sd})",
    "* Information: {signif(x$information, 4)}"
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
    "* Log-likelihood: {round(x$mle$log_lik, 4)}",
    "* Original parameters: {round(x$mle$original, 4)}"
  ))
  invisible(x)
}
