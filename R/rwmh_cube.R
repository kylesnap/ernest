#' Random walk over the unconstrained prior distribution
#'
#' Generate new live points by evolving a current live point through
#' a random walk with a fixed number of steps and an adaptive step
#' size.
#'
#' @param steps (positive integer) Number of steps to take when generating a
#' proposal point.
#' @param target_acceptance (double between 0 and 1) The target acceptance rate
#' for the sampler.
#'
#' @returns A list, with class `c("rwmh_cube", "ernest_lrps")`. Can be used with
#' [nested_sampling()] to specify the sampling behaviour of a nested sampling run.
#'
#' @details
#' Calling [update_lrps()] will build a new proposal distribution based on the
#' acceptance rate of the previous run. The step size \eqn{\epsilon} is updated
#' using a Newton-like method, where the current acceptance rate \eqn{a_{cur}}
#' is used to target the acceptance rate \eqn{a^*}:
#' \deqn{\epsilon_{new}=\epsilon_{old} * \exp((a_{cur} - a^*)/(n_{dim} * a^*))}
#'
#' @references
#' * Original implementation: Skilling, J. (2006). Nested Sampling for General
#' Bayesian Computation. Bayesian Analysis, 1(4), 833–859.
#' <https://doi.org/10.1214/06-BA127>
#' * Adaptive step size implementation: Speagle, J. S. (2020). Dynesty: A
#' Dynamic Nested Sampling Package for Estimating Bayesian Posteriors and
#' Evidences. Monthly Notices of the Royal Astronomical Society, 493, 3132–3158.
#' <https://doi.org/10.1093/mnras/staa278>
#'
#' @export
rwmh_cube <- function(steps = 25, target_acceptance = 0.5) {
  new_rwmh_cube(
    steps = steps,
    target_acceptance = target_acceptance
  )
}

#' Format method for rwmh_cube
#' @param x A `rwmh_cube`
#' @param ... Ignored.
#' @returns A string
#' @noRd
#' @export
format.rwmh_cube <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_h3("Random Walk in Unit Cube LRPS")
    cli::cli_text("# Dimensions: {x$n_dim %||% 'Uninitialized'}")
    cli::cli_text("# Calls Since Update: {x$cache$n_call %||% 0L}")
    cli::cli_text("# Accepted Proposals Since Update: {x$cache$n_acc %||% 0L}")
    cli::cli_text("Current Step Size: {.val {x$cache$epsilon %||% 1}}")
  })
}

#' Create a new rwmh_cube LRPS
#'
#' Internal constructor for the random walk Metropolis-Hastings unit cube LRPS.
#'
#' @param unit_log_fn,n_dim,max_loop,cache,steps,target_acceptance
#' See [new_ernest_lrps] and [rwmh_cube()].
#'
#' @returns An LRPS specification, a list with class
#' `c("rwmh_cube", "ernest_lrps")`.
#' @noRd
new_rwmh_cube <- function(
  unit_log_fn = NULL,
  n_dim = NULL,
  max_loop = 1e6L,
  cache = NULL,
  steps = 25L,
  target_acceptance = 0.5
) {
  check_number_whole(steps, min = 2)
  check_number_decimal(target_acceptance)
  if (target_acceptance < 1 / steps) {
    cli::cli_abort("`target_acceptance` must be at least 1/{steps}.")
  }
  if (target_acceptance >= 1) {
    cli::cli_abort("`target_acceptance` must be smaller than 1.")
  }
  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    steps = steps,
    target_acceptance = target_acceptance,
    .class = "rwmh_cube"
  )
}

#' @rdname propose
#' @export
propose.rwmh_cube <- function(x, original = NULL, criteria = NULL) {
  if (is.null(original)) {
    NextMethod()
  } else {
    cur_call <- env_cache(x$cache, "n_call", 0)
    cur_accept <- env_cache(x$cache, "n_accept", 0)
    res <- RandomWalkMetropolis(
      unit = original,
      criteria = criteria,
      unit_log_lik = x$unit_log_fn,
      num_dim = x$n_dim,
      steps = x$steps,
      epsilon = env_cache(x$cache, "epsilon", 1.0)
    )
    if (any(is.na(res$log_lik))) {
      no_swaps <- which(is.na(res$log_lik))
      res$log_lik[no_swaps] <- x$unit_log_fn(original[no_swaps, ])
    }
    env_poke(x$cache, "n_call", cur_call + res$n_call)
    env_poke(x$cache, "n_accept", cur_accept + res$n_accept)
    res
  }
}

#' @rdname update_lrps
#' @export
update_lrps.rwmh_cube <- function(x) {
  cur_accept <- env_cache(x$cache, "n_accept", 0)
  cur_call <- env_cache(x$cache, "n_call", 0)
  acc_ratio <- cur_accept / cur_call
  # Newton-like update to target the acceptance ratio
  new_epsilon <- env_cache(x$cache, "epsilon", 1.0) *
    exp(
      (acc_ratio - x$target_acceptance) / x$n_dim / x$target_acceptance
    )
  env_poke(x$cache, "epsilon", new_epsilon)
  env_poke(x$cache, "n_accept", 0L)
  env_poke(x$cache, "n_call", 0L)
  do.call(new_rwmh_cube, as.list(x))
}
