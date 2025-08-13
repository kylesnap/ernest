#' Perform nested sampling
#'
#' Execute the nested sampling loop. Iteratively replaces the worst live point
#' in a set with a point drawn from a likelihood-restricted prior sampler,
#' until a provided stopping criterion is met.
#'
#' @param x (ernest_sampler) An object of class `ernest_sampler`.
#' @inheritDotParams compile.ernest_run seed clear
#' @param max_iterations (positive integer) The maximum number of iterations to
#' perform. If set to `Inf`, this stopping criterion is ignored.
#' @param max_calls (positive integer) The maximum number of calls to the
#' likelihood function. If set to `Inf`, this stopping criterion is ignored.
#' @param min_logz (positive double or zero) The minimum ratio between the
#' current log evidence and remaining log evidence (see Details). If set to
#' zero, this stopping criterion is ignored.
#' @param show_progress Whether to show a simple progress spinner and iteration
#' counter during the run.
#'
#' @details
#' At least one of `max_iterations`, `max_calls`, or `min_logz`  must represent
#' a non-ignored stopping criterion.
#'
#' At the iteration \eqn{i}, the remaining log evidence within the prior space
#' bound by a minimum likelihood criterion can be estimated as
#' \deqn{Z^*_i \approx L^{(max)}_i V_i} where \eqn{L^{(max)}_i}
#' is the maximum likelihood of the current live points and \eqn{V_i} is the
#' estimated remaining volume.
#'
#' This estimate can be used to define a relative stopping criterion based on
#' the log-ratio of the current estimated evidence \eqn{\hat{Z_i}} to the
#' remaining evidence, such that
#' \deqn{\delta \log(Z) = \log(\hat{Z_i} - Z^*_i) - \log(\hat{Z_i})}
#' By setting `min_logz`, you can vary the minimum log-ratio at which sampling
#' stops. Once \eqn{\delta \log(Z)} falls below this value, you can assume that
#' only a negligible fraction of the evidence remains unaccounted for in the
#' final evidence estimates.
#'
#' If `x` already contains results from previous calls to `generate()`, then
#' `generate()` will ensure that your stopping criterion have not already been
#' surpassed during previous runs.
#'
#' Control the verbosity of other messages in ernest as it runs with the
#' `rlib_message_verbosity` enviroment option.
#'
#' @returns An object of class [ernest_run-class], containing the results of the
#' nested sampling run.
#' @srrstats {BS2.12, BS2.13} Documentation of the `rlang` verbosity parameter.
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
#' sampler <- nested_sampling(ll_fn, prior, n_point = 100)
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
#' @method generate ernest_sampler
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
