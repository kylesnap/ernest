#' Run nested sampling to estimate Bayesian evidence
#'
#' Executes the nested sampling algorithm, iteratively replacing the worst live
#' point with a new sample from a likelihood-restricted prior until a stopping
#' criterion is met.
#'
#' @param x An object of class `ernest_sampler` or `ernest_run`.
#' @inheritDotParams compile.ernest_run -object
#' @param max_iterations Optional positive integer. The maximum number of
#' iterations to perform. If `NULL`, this criterion is ignored.
#' @param max_calls Optional positive integer. The maximum number of calls to
#' the likelihood function. If `Inf`, this criterion is ignored.
#' @param min_logz Non-negative double. The minimum log-ratio between the
#' current estimated evidence and the remaining evidence. If zero, this
#' criterion is ignored.
#' @param show_progress Logical. If `TRUE`, displays a progress spinner and
#' iteration counter during sampling. If `NULL`, then the parameter is inferred
#' based on the value of the `rlib_message_verbosity` option.
#'
#' @returns An object of class `ernest_run`, inheriting from `ernest_sampler`,
#' with additional components:
#'
#' * `n_iter`: Integer. Number of iterations.
#' * `n_calls`: Integer. Total number of likelihood function calls.
#' * `log_lik`: `double(n_iter + n_points)`. Log-likelihoods for each sample.
#' * `log_volume`: `double(n_iter + n_points)`. Estimated log-prior volumes at
#'   each iteration.
#' * `log_weight`: `double(n_iter + n_points)`. Unnormalised log-weights for
#'   each sample.
#' * `log_evidence`: `double(n_iter + n_points)`. Cumulative log-evidence
#'   estimates at each iteration.
#' * `log_evidence_var`: `double(n_iter + n_points)`. Variance of the
#'   log-evidence estimate at each iteration.
#' * `information`: `double(n_iter + n_points)`. KL divergence between the prior
#'   and posterior, estimated at each iteration.
#' * `samples`: `matrix(nrow = n_iter + n_points, ncol = n_dim)`. Parameter
#'   values for each sample.
#' * `samples_unit`: `matrix(nrow = n_iter + n_points, ncol = n_dim)`. Parameter
#'   values for each sample in unit hypercube representation.
#' * `id`: `integer(n_iter + n_points)`. Unique integer identifiers for each
#'   sample from the live set (ranging from 1 to `n_points`).
#' * `points`: `integer(n_iter + n_points)`. Number of live points present at
#'   each iteration.
#' * `calls`: `integer(n_iter + n_points)`. Number of calls used to generate a
#'   new live point at each iteration.
#' * `birth`: `integer(n_iter + n_points)`. Iteration at which each sample was
#'   first created (ranging from 0 to `n_iter`).
#'
#' @details At least one of `max_iterations`, `max_calls`, or `min_logz` must
#' specify a valid stopping criterion. Setting `min_logz` to zero while leaving
#' `max_iterations` and `max_calls` at their defaults will result in an error.
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
#' @references Skilling, J. (2006). Nested Sampling for General Bayesian
#' Computation. Bayesian Analysis, 1(4), 833–859.
#' <https://doi.org/10.1214/06-BA127>
#'
#' @srrstats {BS2.12} The `show_progress` indicator controls whether a simple
#' spinner bar is shown during sampling.
#' @srrstats {BS4.0} References the paper containing the sampling algorithm.
#' @srrstats {BS5.0, BS5.1, BS5.2} Seed value stored as an attribute. Return
#' values contain the objects used to generate a run, including the prior
#' specification.
#'
#' @examples
#' prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- ernest_sampler(ll_fn, prior, n_point = 100)
#' sampler
#'
#' # Stop sampling after a set number of iterations or likelihood calls.
#' generate(sampler, max_iterations = 100)
#'
#' # The final number of calls may exceed `max_calls`, as `generate`
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
  show_progress = NULL
) {
  if (is.null(show_progress)) {
    show_progress <- getOption("rlib_message_verbosity", "default") != "quiet"
  }
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
#' @srrstats {BS2.8} Calling generate on an ernest_run will continue the run
#' from the last known set of live points.
#' @export
generate.ernest_run <- function(
  x,
  ...,
  max_iterations = NULL,
  max_calls = NULL,
  min_logz = 0.05,
  show_progress = NULL
) {
  if (is.null(show_progress)) {
    show_progress <- getOption("rlib_message_verbosity", "default") != "quiet"
  }
  check_bool(show_progress)
  x <- compile(x, ...)
  if (inherits_only(x, "ernest_sampler")) {
    args <- list2(...)
    return(generate(
      x,
      seed = args$seed,
      max_iterations = max_iterations,
      max_calls = max_calls,
      min_logz = min_logz,
      show_progress = show_progress
    ))
  }

  cur_iter <- x$n_iter
  cur_calls <- x$n_calls
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
      cur_calls,
      d_logz
    )

  results <- nested_sampling_impl(
    x = x,
    max_iterations = max_iterations,
    max_calls = max_calls,
    min_logz = min_logz,
    last_criterion = last_criterion,
    log_vol = log_vol,
    log_z = log_z,
    iter = cur_iter,
    call = cur_calls,
    show_progress = show_progress
  )
  new_ernest_run(x, results)
}

#' Check and validate stopping criteria for nested sampling
#'
#' @param max_iterations Maximum number of iterations to perform.
#' @param max_calls Maximum number of likelihood function calls.
#' @param min_logz Minimum log-ratio between current estimated evidence and
#' remaining evidence.
#' @param cur_it Current iteration.
#' @param cur_cls Current number of likelihood calls.
#' @param d_logz Current log-ratio for evidence.
#' @param call Environment for error reporting.
#'
#' @return A named vector of stopping criteria: `max_iterations`,
#' `max_calls`, and `min_logz`.
#' @noRd
check_stopping_criteria <- function(
  max_iterations,
  max_calls,
  min_logz,
  cur_it = NULL,
  cur_calls = NULL,
  d_logz = NULL,
  call = caller_env()
) {
  check_number_whole(
    max_iterations,
    min = 1,
    allow_null = TRUE,
    allow_infinite = FALSE,
    call = call
  )
  check_number_whole(
    max_calls,
    min = 1,
    allow_null = TRUE,
    allow_infinite = FALSE,
    call = call
  )
  check_number_decimal(
    min_logz,
    min = 0,
    allow_infinite = FALSE,
    call = call
  )

  if (!is.null(cur_it) && !is.null(max_iterations)) {
    if (cur_it >= max_iterations) {
      cli::cli_abort(
        c(
          "`max_iterations` must be strictly larger than {cur_it}.",
          "x" = "`x` already contains previously-generated samples.",
          "i" = "Should you use `clear` to erase previous samples from `x`?"
        ),
        call = call
      )
    }
  }
  if (!is.null(cur_calls) & !is.null(max_calls)) {
    if (cur_calls >= max_calls) {
      cli::cli_abort(
        c(
          "`max_calls` must be strictly larger than {cur_calls}.",
          "x" = "`x` already contains previously-generated samples.",
          "i" = "Should you use `clear` to erase previous samples from `x`?"
        ),
        call = call
      )
    }
  }
  if (!is.null(d_logz) && !is.null(min_logz)) {
    if (min_logz >= d_logz) {
      cli::cli_abort(
        c(
          "`min_logz` must be strictly smaller than {pretty(d_logz)}.",
          "x" = "`x` already contains previously-generated samples.",
          "i" = "Should you use `clear` to erase previous samples from `x`?"
        ),
        call = call
      )
    }
  }

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

  c(
    "max_iterations" = as.integer(max_iterations %||% .Machine$integer.max),
    "max_calls" = as.integer(max_calls %||% .Machine$integer.max),
    "min_logz" = as.double(min_logz)
  )
}
