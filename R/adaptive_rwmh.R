#' Generate samples with an adaptive random walk
#'
#' Create new live points by evolving points with a Metropolis-Hastings random
#' walk. The algorithm simultaneously adapts both the step size and
#' covariance structure of the proposal distribution to target a
#' specific acceptance rate.
#'
#' @param steps Positive integer. A number of steps to take when generating a
#' proposal point.
#' @param target_acceptance A number between `1 / steps` and 1.0. Target
#' acceptance rate for proposed points.
#' @param min_epsilon Number equal to or larger than 0. Smallest step-size
#' possible for the sampler to generate.
#' @param strength Positive number controlling the strength of prior
#' information for covariance matrix estimation. When `NULL`, this is
#' set using the number of live points.
#' @param forgetfulness Number between 0 and 1 controlling how quickly past
#' samples are forgotten when estimating the covariance matrix.
#'
#' @returns An object of class `c("adaptive_rwmh", "ernest_lrps")` that can be
#' used with [ernest_sampler()] to specify the sampling behaviour.
#'
#' @details
#' The adaptive random walk LRPS is a modification of the algorithm offered by
#' Spencer (2021). Like [rwmh_cube], a random walk with a fixed number of
#' steps is used to evolve `original` to some new point with likelihood greater
#' than `criterion`. Unique to `adaptive_rwmh`, each step taken contributes to
#' the size and shape of the proposal distribution. Theoretically, this allows
#' the proposals to more readily adapt to local features of the posterior
#' distribution.
#'
#' **Shaping**: The covariance matrix is estimated using the accepted
#' point at each iteration. To stabilise the estimates, the included points
#' are collected in a sliding window. The forgetting function
#' \eqn{f(i) = \lfloor \text{forgetfulness} \times i \rfloor} determines when
#' the window shifts, replacing the contribution of the sample at index
#' \eqn{f(i-1)} with the sample at index \eqn{f(i)}. Given the
#' previously-accepted sample \eqn{X_{i-1}}, the covariance matrix estimate
#' \eqn{\Sigma_i}, and the number of dimensions \eqn{d}, the proposed point is
#' generated from:
#'
#' \deqn{N\left(X_{i-1}, \epsilon \cdot \frac{2.38^2}{d} \cdot \Sigma_i\right)}
#'
#' Calling [update_lrps] restimates the covariance matrix of the points with
#' [stats::cov()].
#'
#' **Scale**: Step-size \eqn{\epsilon} is adapted using a Robbins-Monro
#' stochastic approximation to achieve the target acceptance rate
#' \eqn{\alpha_*}. At iteration \eqn{i}, with an actual acceptance rate of
#' \eqn{\alpha_i} and a scaling parameter \eqn{\delta}, the step size is updated
#' with
#'
#' \deqn{\log(\epsilon_i) + \frac{\delta}{i} (\alpha_i - \alpha_*)}
#'
#' \eqn{\epsilon_i} starts at 1 and is always at least `min_epsilon`.
#'
#' @section Control Parameters:
#' * `steps`: Start with 25-50. Increase to generate samples that more closely
#' follow the posterior distribution; decrease for computational efficiency.
#' * `target_acceptance`: Start with 0.4-0.6. Smaller values encourage
#' the sampler to perform more global explorations of the posterior
#' distribution.
#' * `min_epsilon`: Start with 0.1. Smaller values allow proposals to wander in
#' smaller regions of the posterior distribution, but may restrict the
#' distance between the random walk's start and end points.
#' * `strength`: Use `NULL` or start with 100. Larger values will
#' promote inter-sample stability of the covariance matrix; smaller values may
#' be helpful for navigating posterior surfaces with local features.
#' * `forgetfulness`: Start with 0.2-0.6. Smaller values may be helpful for
#' quickly adapting to local features of the posterior distribution.
#'
#' @references
#' Spencer, S.E.F. (2021) Accelerating Adaptation in the Adaptive
#' Metropolis–Hastings Random Walk Algorithm. Aust. N. Z. J. Stat., 63: 468-484.
#' <https://doi.org/10.1111/anzs.12344>
#'
#' @examples
#' # Basic usage with default parameters
#' lrps <- adaptive_rwmh()
#'
#' # Conservative settings for difficult problems
#' robust_lrps <- adaptive_rwmh(
#'   steps = 100,
#'   target_acceptance = 0.5,
#'   strength = 50,
#'   forgetfulness = 0.6
#' )
#'
#' @family ernest_lrps
#' @export
adaptive_rwmh <- function(
  steps = 25,
  target_acceptance = 0.4,
  min_epsilon = 0.1,
  strength = NULL,
  forgetfulness = 0.4
) {
  new_adaptive_rwmh(
    steps = steps,
    target_acceptance = target_acceptance,
    min_epsilon = min_epsilon,
    strength = strength,
    forgetfulness = forgetfulness
  )
}

#' @noRd
#' @export
format.adaptive_rwmh <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_text("adaptive random walk LRPS {.cls {class(x)}}")
    cli::cat_line()
    cli::cli_text("No. Dimensions: {x$n_dim %||% 'Uninitialized'}")
    cli::cli_text("Current Step Size: {pretty(x$cache$epsilon %||% 1)}")
    cli::cli_text("Target Acceptance: {x$target_acceptance}")
  })
}

#' Create a new adaptive_rwmh LRPS
#'
#' Internal constructor for the adaptive random walk Metropolis-Hastings LRPS.
#'
#' @param unit_log_fn Function for computing log-likelihood in unit space.
#' @param n_dim Integer. Number of dimensions.
#' @param max_loop Integer. Maximum number of proposal attempts.
#' @param cache Optional cache environment.
#' @param steps Integer. Number of steps in the random walk.
#' @param target_acceptance Numeric. Target acceptance rate for proposals.
#' @param min_epsilon Numeric. Minimum step size parameter.
#' @param strength_nu Numeric. Strength of prior information for covariance.
#' @param forget_b Numeric. Forgetfulness parameter.
#'
#' @return An LRPS specification, a list with class
#' `c("adaptive_rwmh", "ernest_lrps")`.
#' @noRd
new_adaptive_rwmh <- function(
  unit_log_fn = NULL,
  n_dim = NULL,
  max_loop = 1e6L,
  cache = NULL,
  steps = 25L,
  target_acceptance = 0.4,
  min_epsilon = 0.1,
  strength = NULL,
  forgetfulness = 0.4
) {
  check_number_whole(steps, min = 2)
  check_number_decimal(target_acceptance, max = 1)
  if (target_acceptance < 1 / steps) {
    cli::cli_abort("`target_acceptance` must be at least 1/{steps}.")
  }
  check_number_decimal(min_epsilon, min = 0)
  check_number_decimal(forgetfulness, min = 0, max = 1)

  if (!is.null(strength)) {
    check_number_decimal(strength, min = 0)
  }

  cache <- cache %||% new_environment()
  env_poke(cache, "n_call", 0L)
  env_poke(cache, "n_accept", 0L)
  env_cache(cache, "epsilon", 1.0)
  if (is_scalar_integerish(n_dim) && n_dim > 0L) {
    env_cache(cache, "mean", rep(0.5, n_dim))
    env_cache(cache, "covariance", diag(1 / 12, nrow = n_dim))
  }

  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    steps = as.integer(steps),
    target_acceptance = as.double(target_acceptance),
    min_epsilon = as.double(min_epsilon),
    strength = if (is.null(strength)) NULL else as.double(strength),
    forgetfulness = as.double(forgetfulness),
    .class = "adaptive_rwmh"
  )
}

#' @rdname propose
#' @export
propose.adaptive_rwmh <- function(
  x,
  original = NULL,
  criterion = -Inf,
  idx = NULL
) {
  if (is.null(original)) {
    NextMethod(x)
  } else {
    res <- AdaptiveRWImpl(
      original = original,
      unit_log_fn = x$unit_log_fn,
      criterion = criterion,
      steps = x$steps,
      epsilon_init = x$cache$epsilon %||% 1.0,
      epsilon_min = x$min_epsilon,
      target_acceptance = x$target_acceptance,
      mean = x$cache$mean,
      cov = x$cache$covariance,
      strength = x$strength %||% 100,
      forgetfulness = x$forgetfulness
    )

    # Update cache with new adaptive parameters
    env_poke(x$cache, "n_call", x$cache$n_call + res$n_call)
    env_poke(x$cache, "n_accept", x$cache$n_accept + res$n_accept)
    env_bind(
      x$cache,
      mean = res$mean,
      covariance = res$cov,
      epsilon = res$epsilon
    )
    res
  }
}

#' @rdname update_lrps
#' @export
update_lrps.adaptive_rwmh <- function(x, unit = NULL) {
  if (is.null(unit)) {
    return(do.call(new_adaptive_rwmh, as.list(x)))
  }
  try_fetch(
    {
      x$strength <- x$strength %||% nrow(unit)
      cov <- stats::cov(unit)
      mean <- colMeans(unit)
      env_poke(x$cache, "mean", mean)
      env_poke(x$cache, "covariance", cov)
    },
    error = function(cnd) {
      cli::cli_warn(
        "Using the identity matrix after encountering a rebounding error.",
        parent = cnd
      )
      x$strength <- NULL
      env_unbind(x$cache, c("covariance", "epsilon", "mean"))
    }
  )
  do.call(new_adaptive_rwmh, as.list(x))
}
