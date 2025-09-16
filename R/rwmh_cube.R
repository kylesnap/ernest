#' Generate samples with a random walk
#'
#' Generate new live points by evolving a current live point through
#' a Metropolis-Hastings random walk, rejecting steps that fail to meet the
#' likelihood criterion.
#'
#' @param steps Positive integer. Number of steps to take when generating a
#' proposal point.
#' @param target_acceptance Double between `1 / steps` and 1.0. The target
#' acceptance rate for the proposed points.
#' @param cov_fn An optional function. If provided, `cov_fn` should
#' be able to return the variance-covariance matrix for a set of live points
#' in the sampler. This is used to scale the steps during sampling. If set to
#' `NULL`, sampling steps will ignore the covariance of the live points.
#'
#' @returns A list with class `c("rwmh_cube", "ernest_lrps")`. Can be used with
#' [ernest_sampler()] to specify the sampling behaviour of a nested sampling
#' run.
#'
#' @details
#' Calling [update_lrps()] builds a new proposal distribution based on the
#' acceptance rate of the previous run. The step size \eqn{\epsilon} is updated
#' using a Newton-like method, where the current acceptance rate \eqn{a_{cur}}
#' is used to target the acceptance rate \eqn{a^*}:
#' \deqn{\epsilon_{new} = \epsilon_{old} * \exp((a_{cur} - a^*)/(n_{dim} * a^*))}
#'
#' @srrstats {G3.1, G3.1a} Users may choose their own cov() method or disable
#' it entirely. This is tested.
#'
#' @references
#' * Skilling, J. (2006). Nested Sampling for General
#'   Bayesian Computation. Bayesian Analysis, 1(4), 833–859.
#'   <https://doi.org/10.1214/06-BA127>
#' * Speagle, J. S. (2020). Dynesty: A Dynamic Nested Sampling Package for
#'   Estimating Bayesian Posteriors and Evidences. Monthly Notices of the
#'   Royal Astronomical Society, 493, 3132–3158.
#'   <https://doi.org/10.1093/mnras/staa278>
#'
#' @srrstats {BS4.0} References the paper and software containing the sampling
#' algorithm.
#' @examples
#' data(example_run)
#' lrps <- rwmh_cube()
#'
#' ernest_sampler(example_run$log_lik_fn, example_run$prior, sampler = lrps)
#'
#' # Change the default behaviour of the sampler:
#' rwmh_cube(steps = 20, target_acceptance = 0.4)
#' @family ernest_lrps
#' @export
rwmh_cube <- function(
  steps = 25,
  target_acceptance = 0.5,
  cov_fn = stats::cov
) {
  if (!is.null(cov_fn)) {
    cov_fn = as_closure(cov_fn)
  }
  new_rwmh_cube(
    steps = steps,
    target_acceptance = target_acceptance,
    cov_fn = cov_fn
  )
}

#' @noRd
#' @export
format.rwmh_cube <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_text("Random Walk in Unit Cube LRPS {.cls {class(x)}}")
    cli::cli_text("No. Dimensions: {x$n_dim %||% 'Uninitialized'}")
    cli::cli_text("No. Calls Since Update: {x$cache$n_call %||% 0L}")
    cli::cli_text("No. Accepted Since Update: {x$cache$n_acc %||% 0L}")
    cli::cli_text("Current Step Size: {pretty(x$cache$epsilon %||% 1)}")
  })
}

#' Create a new rwmh_cube LRPS
#'
#' Internal constructor for the random walk Metropolis-Hastings unit cube LRPS.
#'
#' @param unit_log_fn Function for computing log-likelihood in unit space.
#' @param n_dim Integer. Number of dimensions.
#' @param max_loop Integer. Maximum number of proposal attempts.
#' @param cache Optional cache environment.
#' @param steps Integer. Number of steps in the random walk.
#' @param target_acceptance Numeric. Target acceptance rate for proposals.
#' @param cov_fn A call to compute the covariance matrix.
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
  target_acceptance = 0.5,
  cov_fn = NULL
) {
  check_number_whole(steps, min = 2)
  check_number_decimal(target_acceptance)
  check_closure(cov_fn, allow_null = TRUE)
  if (target_acceptance < 1 / steps) {
    cli::cli_abort("`target_acceptance` must be at least 1/{steps}.")
  }
  if (target_acceptance >= 1) {
    cli::cli_abort("`target_acceptance` must be smaller than 1.")
  }
  cache <- cache %||% new_environment()
  env_cache(cache, "n_accept", 0L)
  if (is_scalar_integerish(n_dim)) {
    env_cache(cache, "chol_cov", diag(n_dim))
  }
  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    steps = as.integer(steps),
    target_acceptance = as.double(target_acceptance),
    cov_fn = cov_fn,
    .class = "rwmh_cube"
  )
}

#' @rdname propose
#' @export
propose.rwmh_cube <- function(
  x,
  original = NULL,
  criterion = -Inf,
  idx = NULL
) {
  if (is.null(original)) {
    NextMethod(x)
  } else {
    res <- RandomWalkImpl(
      original = original,
      log_lik_fn = x$unit_log_fn,
      criterion = criterion,
      steps = x$steps,
      epsilon = env_cache(x$cache, "epsilon", 1.0),
      chol_cov = env_cache(x$cache, "chol_cov", diag(x$n_dim))
    )
    env_poke(x$cache, "n_call", x$cache$n_call + res$n_call)
    env_poke(x$cache, "n_accept", x$cache$n_accept + res$n_accept)
    res
  }
}

#' @rdname update_lrps
#' @export
update_lrps.rwmh_cube <- function(x, unit = NULL) {
  # Newton-like update to epsilon based on the acceptance ratio
  cur_call <- NULL
  cur_accept <- NULL
  cur_eps <- NULL
  c(cur_call, cur_accept, cur_eps) %<-%
    env_get_list(x$cache, c("n_call", "n_accept", "epsilon"), c(0, 0, 1))
  if (cur_call != 0L) {
    acc_ratio <- cur_accept / cur_call
    new_eps <- cur_eps *
      exp((acc_ratio - x$target_acceptance) / x$n_dim / x$target_acceptance)
    env_poke(x$cache, "epsilon", new_eps)
  }
  env_poke(x$cache, "n_call", 0L)
  env_poke(x$cache, "n_accept", 0L)

  # Calculate Cholesky-Decomposed Covariance Matrix
  if (!is.null(x$cov_fn) && !is.null(unit)) {
    chol_cov <- try_fetch(
      {
        tmp <- x$cov_fn(unit)
        check_matrix(tmp, nrow = x$n_dim, ncol = x$n_dim)
        chol(tmp)
      },
      error = function(cnd) {
        cli::cli_warn(
          "Using an identity matrix after failed.",
          parent = cnd
        )
        diag(x$n_dim)
      }
    )
    env_poke(x$cache, "chol_cov", chol_cov)
  }
  do.call(new_rwmh_cube, as.list(x))
}
