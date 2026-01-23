#' Generate new points with a random walk
#'
#' Create new samples for the live set by evolving a current point in the set
#' through a Metropolis-Hastings random walk, rejecting steps that fail to meet
#' the likelihood criterion.
#'
#' @param steps `[integer(1)]`\cr Number of steps to take when generating a
#' proposal point. Must be greater or equal to 2.
#' @param target_acceptance `[double(1)]`\cr Target acceptance rate for proposed
#' points. Must be a number between `1 / steps` and 1.
#'
#' @returns `[rwmh_cube]`, a named list that inherits from [[ernest_lrps]].
#'
#' @details
#' The random walk LRPS generates proposals by performing a fixed number of
#' Metropolis-Hastings steps within the unit hypercube. Each step proposes a
#' new location by adding a random perturbation to the current position,
#' accepting or rejecting the step based on whether it satisfies the likelihood
#' criterion. This process continues for the specified number of steps,
#' with the final accepted position returned as the proposal.
#'
#' **Step-size Adaptation**: The step size \eqn{\epsilon} is adapted between
#' sampling rounds using [update_lrps()]. The adaptation uses a Newton-like
#' method to target the desired acceptance rate \eqn{\alpha_*}. Given the
#' current acceptance rate \eqn{\alpha_i} and number of dimensions
#' \eqn{d}, the step size is updated with:
#'
#' \deqn{\epsilon_i * \exp(\frac{\alpha_i - \alpha^*}{d \cdot \alpha_*})}
#'
#' Given the previously-accepted sample \eqn{X_{i-1}} and the number of
#' dimensions \eqn{d}, proposed points are generated from:
#'
#' \deqn{X_{i-1} + S_d(0, \epsilon)}
#'
#' where \eqn{S(0, \epsilon)} is a point drawn uniformly from the
#' $d$-dimensional ball centered on the origin with radius \eqn{\epsilon}.
#'
#' @section Control Parameters:
#' * `steps`: Start with 25. Increase to generate points that more closely
#' follow the posterior distribution; decrease for computational efficiency.
#' * `target_acceptance`: Start with 0.4-0.6. Lower values encourage more global
#' exploration of the posterior, higher values encourage explorations close
#' to the starting point.
#'
#' @references
#' * Skilling, J. (2006). Nested Sampling for General
#'   Bayesian Computation. Bayesian Analysis, 1(4), 833–859.
#'   \doi{10.1214/06-BA127}
#' * Speagle, J. S. (2020). Dynesty: A Dynamic Nested Sampling Package for
#'   Estimating Bayesian Posteriors and Evidences. Monthly Notices of the
#'   Royal Astronomical Society, 493, 3132–3158.
#'   \doi{10.1093/mnras/staa278}
#'
#' @examples
#' # Basic usage with default parameters
#' lrps <- rwmh_cube()
#'
#' # A faster sampler for simple-to-traverse posterior surfaces
#' fast_lrps <- rwmh_cube(
#'   steps = 20,
#'   target_acceptance = 0.7
#' )
#'
#' @srrstats {BS4.0} References the paper and software containing the sampling
#' algorithm.
#' @family ernest_lrps
#' @export
rwmh_cube <- function(
  steps = 25,
  target_acceptance = 0.5
) {
  new_rwmh_cube(
    steps = steps,
    target_acceptance = target_acceptance
  )
}

#' @noRd
#' @export
format.rwmh_cube <- function(x, ...) {
  acc_per <- pretty_round(100 * x$target_acceptance, 1)
  cli::format_inline(
    "{x$steps}-step random walk sampling (acceptance target = {acc_per}%)"
  )
}

#' Create a new rwmh_cube LRPS
#'
#' Internal constructor for the random walk Metropolis-Hastings unit cube LRPS.
#'
#' @param unit_log_fn Function for computing log-likelihood in unit space.
#' @param n_dim  Number of dimensions.
#' @param max_loop  Maximum number of proposal attempts.
#' @param cache Optional cache environment.
#' @param steps  Number of steps in the random walk.
#' @param target_acceptance Numeric. Target acceptance rate for proposals.
#'
#' @srrstats {G2.4, G2.4a, G2.4b} Explicit conversion of inputs to expected
#' types or error messages for univariate inputs.
#' @srrstats {BS2.6} Ensures that both control parameters for random walks
#' are within plausible ranges.
#'
#' @return An LRPS specification, a list with class
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
  if (target_acceptance < (1 / steps)) {
    cli::cli_abort("`target_acceptance` must be at least 1/{steps}.")
  }
  if (target_acceptance >= 1) {
    cli::cli_abort("`target_acceptance` must be smaller than 1.")
  }
  cache <- cache %||% new_environment()
  env_poke(cache, "n_accept", 0L)
  env_cache(cache, "epsilon", 1.0)
  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    steps = as.integer(steps),
    target_acceptance = as.double(target_acceptance),
    .class = "rwmh_cube"
  )
}

#' @rdname propose
#' @export
propose.rwmh_cube <- function(
  x,
  original = NULL,
  criterion = -Inf
) {
  if (is.null(original)) {
    NextMethod(x)
  } else {
    res <- RandomWalkImpl(
      original = original,
      unit_log_fn = x$unit_log_fn,
      criterion = criterion,
      steps = x$steps,
      epsilon = x$cache$epsilon
    )
    env_poke(x$cache, "neval", x$cache$neval + res$neval)
    env_poke(x$cache, "n_accept", x$cache$n_accept + res$n_accept)
    res
  }
}

#' @rdname update_lrps
#' @export
update_lrps.rwmh_cube <- function(x, unit = NULL, ...) {
  if (!is.matrix(unit)) {
    return(do.call(new_rwmh_cube, as.list(x)))
  }
  # Newton-like update to epsilon based on the acceptance ratio
  cur_call <- env_get(x$cache, "neval", 0L)
  cur_accept <- env_get(x$cache, "n_accept", 0L)
  cur_eps <- env_get(x$cache, "epsilon", 1.0)
  if (cur_call != 0L) {
    acc_ratio <- cur_accept / cur_call
    new_eps <- cur_eps *
      exp((acc_ratio - x$target_acceptance) / x$n_dim / x$target_acceptance)
    env_poke(x$cache, "epsilon", new_eps)
  }
  do.call(new_rwmh_cube, as.list(x))
}
