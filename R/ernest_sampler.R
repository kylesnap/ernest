#' Prepare a new nested sampling run
#'
#' Initializes an `ernest_sampler` object containing the components required to
#' perform nested sampling. This object can then be used to build sequences of
#' nested samples with [generate()].
#'
#' @param log_lik A function which takes a vector of parameters as a vector and
#' returns the log-likelihood of a given model. Wrapped using
#' [create_likelihood()] unless it is already of class `ernest_likelihood`.
#' @param prior An object with class [ernest_prior]. Describes the prior space
#' within which to generate sample parameters for `log_lik`.
#' @param sampler An object of class [ernest_lrps]. Describes the
#' likelihood-restricted prior sampling technique to adopt during the run.
#' @param n_points A strictly positive integer. The number of live points to use
#' in the nested sampling run.
#' @param first_update An optional positive integer. The number of likelihood
#' calls to make with the default [uniform LRPS][unif_cube()] method before
#' swapping to the technique described by `sampler`. If left `NULL`,
#' this is set to `n_points * 2.5`.
#' @param update_interval An optional positive integer. The number of likelihood
#' calls between updates to the `sampler` object. If `NULL`, this is set to
#' `n_points * 1.5`.
#'
#' @return An object of class `ernest_sampler`, which is a list containing the
#' inputs used as arguments to this function, along with an environment
#' `run_env` which is used to store the `n_points` live particles throughout
#' a nested sampling run.
#'
#' @details
#' The `ernest_sampler` object is tested with [compile()] before it is
#' returned. This helps to catch errors with the likelihood and prior
#' specifications. If this compilation step fails, review your `log_lik_fn` and
#' `prior` objects for their compliance.
#'
#' ## Verbosity
#' Messages from ernest can be silenced with the global options
#' `rlib_message_verbosity` and `rlib_warning_verbosity`. These options take the
#' values:
#'
#' * "default": Verbose unless the `.frequency` argument is supplied.
#' * "verbose": Always verbose.
#' * "quiet": Always quiet.
#'
#' When set to quiet, message are not displayed and the condition is not
#' signaled. See [rlang::abort()] for more information.
#'
#' @seealso
#' * [create_likelihood()] describes the requirements of the `log_lik_fn`
#'   parameter.
#' * [create_prior()] describes the requriements of the `prior` parameter.
#' * [ernest_lrps] describes the general requirements of likelihood-restricted
#' * prior samplers.
#'
#' @srrstats {BS1.3, BS1.3b} Describes how to set different sampling techniques.
#' @srrstats {G2.13, BS2.1, BS2.4} Both `compile()` and the checks performed by
#' ernest_sampler catch missing values or incommensurability between the
#' prior and likelihood. This helps avoid calls to `generate()` with malformed
#' samplers.
#' @srrstats {BS2.12} Documents verbosity settings for all progress messages
#' and warnings within ernest.
#'
#' @export
#' @examples
#' prior <- create_uniform_prior(n_dim = 2, lower = -1, upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- ernest_sampler(ll_fn, prior, n_points = 100)
#' sampler
#'
#' # Use a unit-cube LRPS (not recommended in practice)
#' unit_sampler <- ernest_sampler(
#'   ll_fn,
#'   prior,
#'   n_points = 100,
#'   sampler = unif_cube
#' )
#' unit_sampler
ernest_sampler <- function(
  log_lik,
  prior,
  sampler = rwmh_cube(),
  n_points = 500,
  first_update = NULL,
  update_interval = NULL
) {
  if (!inherits(log_lik, "ernest_likelihood")) {
    log_lik <- create_likelihood(fn = log_lik)
  }
  obj <- new_ernest_sampler(
    log_lik_fn = log_lik,
    prior = prior,
    lrps = sampler,
    n_points = n_points,
    first_update = first_update %||% as.integer(n_points * 2.5),
    update_interval = update_interval %||% as.integer(n_points * 1.5)
  )

  try_fetch(
    {
      live <- create_live(obj$lrps, obj$n_points)
      env_bind(
        obj$run_env,
        "unit" = live$unit,
        "log_lik" = live$log_lik,
        "birth" = rep(0L, obj$n_points)
      )
      check_live_set(obj)
      obj$live_point <- new_environment()
    },
    error = function(cnd) {
      cli::cli_abort(
        "{.cls ernest_sampler} cannot compile.",
        parent = cnd
      )
    },
    warning = function(cnd) {
      cli::cli_warn(
        "{.cls ernest_sampler} threw a warning during compilation",
        parent = cnd
      )
    }
  )
  obj
}
