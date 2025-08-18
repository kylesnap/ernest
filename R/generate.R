#' Run nested sampling to estimate Bayesian evidence
#'
#' Executes the nested sampling algorithm, iteratively replacing the worst live
#' point with a new sample from a likelihood-restricted prior, until a stopping
#' criterion is met.
#'
#' @param x An object of class `ernest_sampler` or `ernest_run`.
#' @inheritDotParams compile.ernest_run -object
#' @param max_iterations An optional strictly positive integer. The maximum
#' number of iterations to perform. If left `NULL`, this criterion is ignored.
#' @param max_calls An optional strictly positive integer. The maximum number
#' of calls to the likelihood function. If set to `Inf`, this criterion is
#' ignored.
#' @param min_logz A non-negative double. The minimum log-ratio between the
#' current estimated evidence and the remaining evidence. If set to zero, this
#' criterion is ignored.
#' @param show_progress Logical. If `TRUE`, displays a progress spinner and
#' iteration counter during sampling.
#'
#' @returns
#' An object of class `ernest_run`, which inherits from `ernest_sampler` and
#' contains these additional components:
#'
#' * `n_iter`: Integer. Number of iterations.
#' * `n_calls`: Integer. Total number of likelihood function calls.
#' * `log_lik`: `double(n_iter + n_points)`. The
#' log-likelihoods for each sample.
#' * `log_volume`: `double(n_iter + n_points)`. The estimated
#' log-prior volumes at each iteration.
#' * `log_weight`: `double(n_iter + n_points)`. The
#' unnormalized log-weights for each sample.
#' * `log_evidence`: `double(n_iter + n_points)`. The
#' cumulative log-evidence estimates at each iteration.
#' * `log_evidence_var`: `double(n_iter + n_points)`. The
#' variance of the log-evidence estimate at each iteration.
#' * `information`: `double(n_iter + n_points)`. The KL
#' divergence between the prior and posterior, estimated at each iteration.
#' * `samples`: `matrix(nrow = n_iter + n_points, ncol = n_dim)`. The parameter
#' values of each sample.
#' * `samples_unit`: `matrix(nrow = n_iter + n_points, ncol = n_dim)`. The
#' parameter values of each sample, in their unit hypercube representation.
#' * `id`: `integer(n_iter + n_points)`. The unique integer identifiers for each
#' sample from the live set (ranging from 1 to `n_points`).
#' * `points`: `integer(n_iter + n_points)`. The number of live points present
#' at each iteration.
#' * `calls`: `integer(n_iter + n_points)`. The number of calls used to generate
#' a new live point at each iteration.
#' * `birth`: `integer(n_iter + n_points)`. The iteration at which each sample
#' was first created (ranging from 0 to `n_iter`).
#'
#' @details
#' At least one of `max_iterations`, `max_calls`, or `min_logz` must
#' indicated a valid stopping criterion. Setting `min_logz` to zero while
#' not changing `max_iterations` or `max_calls` from their defaults will cause
#' an error.
#'
#' If `x` is an `ernest_run` object, the stopping criteria are checked against
#' the current state of the run. An error is thrown if the stopping criteria
#' have already been satisfied by `x`.
#'
#' The `min_logz` parameter controls the relative tolerance for the remaining
#' evidence in the unexplored parameter space. Sampling stops when the estimated
#' remaining evidence is sufficiently small compared to the accumulated
#' evidence.
#'
#' @srrstats {BS4.3, BS4.4, BS4.5} Ernest defaults to using `min_logz` to
#' halt the nested sampling loop when the remaining evidence in the unexplored
#' parameter space is sufficiently small. `max_iterations` is used to prevent
#' infinite loops when `min_logz` cannot be reached.
#' @srrstats {BS1.3, BS1.3a, BS2.8} Describes the stopping criteria used for
#' nested sampling, and details how the results of previous runs can be
#' used as starting points for subsequent runs.
#' @examples
#' prior <- create_uniform_prior(n_dim = 2, lower = -1, upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- ernest_sampler(ll_fn, prior, n_point = 100)
#' sampler
#'
#' # Stop sampling after a set number of iterations or calls to the lik. func.
#' generate(sampler, max_iterations = 100)
#'
#' # The final number of calls may be larger than `max_calls`, as `generate`
#' # only checks the number of calls when removing a live point.
#' generate(sampler, max_calls = 2600)
#'
#' # Use the default stopping criteria
#' \dontrun{ generate(sampler) }
#' @aliases ernest_run
#' @export
generate.ernest_sampler <- function(
  x,
  ...,
  max_iterations = NULL,
  max_calls = NULL,
  min_logz = 0.05,
  show_progress = FALSE
) {
  check_bool(show_progress)
  c(max_iterations, max_calls, min_logz) %<-%
    check_stopping_criteria(
      max_iterations,
      max_calls,
      min_logz
    )
  x <- compile(x, ...)

  results <- nested_sampling_impl(
    x = x,
    max_iterations = max_iterations,
    max_calls = max_calls,
    min_logz = min_logz,
    show_progress = show_progress
  )
  new_ernest_run(x, results)
}

#' @method generate ernest_run
#' @export
generate.ernest_run <- function(
  x,
  ...,
  max_iterations = NULL,
  max_calls = NULL,
  min_logz = 0.05,
  show_progress = FALSE
) {
  check_bool(show_progress)
  cur_iter <- x$n_iter
  cur_cls <- x$n_calls
  last_criterion <- x$log_lik[cur_iter]
  log_z <- x$log_evidence[cur_iter]
  log_vol <- x$log_volume[cur_iter]
  d_logz <- logaddexp(0, max(x$log_lik) + log_vol - log_z)
  c(max_iterations, max_calls, min_logz) %<-%
    check_stopping_criteria(
      max_iterations,
      max_calls,
      min_logz,
      cur_iter,
      cur_cls,
      d_logz
    )
  x <- compile(x, ...)

  results <- nested_sampling_impl(
    x = x,
    max_iterations = max_iterations,
    max_calls = max_calls,
    min_logz = min_logz,
    last_criterion = last_criterion,
    log_vol = log_vol,
    log_z = log_z,
    iter = cur_iter,
    call = cur_cls,
    show_progress = show_progress
  )
  new_ernest_run(x, results)
}

#' Check the stopping criteria for a proposed nested sampling run
#' @param max_iterations,max_calls,min_logz User-requested stopping values
#' @param cur_it,cur_cls,d_logz Minimum stopping values derived from
#' previous runs, if available.
#' @param call Contextual error info.
#' @returns A named vector of stopping criteria.
#' @noRd
check_stopping_criteria <- function(
  max_iterations,
  max_calls,
  min_logz,
  cur_it = 0,
  cur_cls = 0,
  d_logz = NULL,
  call = caller_env()
) {
  check_number_whole(
    max_iterations,
    min = cur_it + 1,
    allow_null = TRUE,
    allow_infinite = FALSE,
    call = call
  )
  check_number_whole(
    max_calls,
    min = cur_cls + 1,
    allow_null = TRUE,
    allow_infinite = FALSE,
    call = call
  )
  check_number_decimal(
    min_logz,
    min = 0,
    max = (d_logz %||% Inf) - .Machine$double.eps,
    call = call
  )

  no_stopping <- isTRUE(all.equal(
    c(max_iterations, max_calls, min_logz),
    c(NULL, NULL, 0)
  ))
  if (no_stopping) {
    cli::cli_abort(
      c(
        "Can't perform nested sampling without any stopping criteria.",
        "i" = "Have you set either `max_iterations` or `max_calls`?"
      ),
      call = call
    )
  }

  max_iterations <- max_iterations %||% .Machine$integer.max
  max_calls <- max_calls %||% .Machine$integer.max
  c(
    "max_iterations" = as.integer(max_iterations),
    "max_calls" = as.integer(max_calls),
    "min_logz" = as.double(min_logz)
  )
}
