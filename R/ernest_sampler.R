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
#' @param nlive A strictly positive integer. The number of live points to use
#' in the nested sampling run.
#' @param first_update An optional positive integer. The number of likelihood
#' calls to make with the default [uniform LRPS][unif_cube()] method before
#' swapping to the technique described by `sampler`. If left `NULL`,
#' this is set to `nlive * 2.5`.
#' @param update_interval An optional positive integer. The number of likelihood
#' calls between updates to the `sampler` object. If `NULL`, this is set to
#' `nlive * 1.5`.
#' @param seed An optional integer. Sets the random seed controlling the
#' random number generator for nested sampling runs, which is stored in
#' the resulting `ernest_sampler` as an attribute. If `NULL`, this is
#' set to a random integer.
#'
#' @return An object of class `ernest_sampler`, which is a list containing the
#' inputs used as arguments to this function, along with an environment
#' `run_env` which is used to store the `nlive` live particles throughout
#' a nested sampling run.
#'
#' @details
#' The `ernest_sampler` object is tested with [compile()] before it is
#' returned. This helps to catch errors with the likelihood and prior
#' specifications. If this compilation step fails, review your `log_lik_fn` and
#' `prior` objects for their compliance.
#'
#' @section Verbosity:
#' Messages from ernest can be silenced with the global options
#' `rlib_message_verbosity` and `rlib_warning_verbosity`. These options take the
#' values:
#'
#' * "default": Verbose unless the `.frequency` argument is supplied.
#' * "verbose": Always verbose.
#' * "quiet": Always quiet.
#'
#' When set to quiet, messages are not displayed and the condition is not
#' signaled. See [rlang::abort()] for more information.
#'
#' @seealso
#' * [create_likelihood()] describes the requirements of the `log_lik_fn`
#'   parameter.
#' * [create_prior()] describes the requriements of the `prior` parameter.
#' * [ernest_lrps] describes the general requirements of likelihood-restricted
#' prior samplers.
#'
#' @srrstats {BS1.3, BS1.3b} Describes how to set different sampling techniques.
#' @srrstats {G2.13, BS2.1, BS2.4} Both `compile()` and the checks performed by
#' ernest_sampler catch missing values or incommensurability between the
#' prior and likelihood. This helps avoid calls to `generate()` with malformed
#' samplers.
#' @srrstats {BS2.12} Documents verbosity settings for all progress messages
#' and warnings within ernest.
#' @srrstats {BS5.0, BS5.1, BS5.2} Seed value stored as an attribute. Return
#' values contain the objects used to generate a run, including the prior
#' specification.
#'
#' @export
#' @examples
#' prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- ernest_sampler(ll_fn, prior, nlive = 100)
#' sampler
#'
#' # Use a unit-cube LRPS (not recommended in practice)
#' unit_sampler <- ernest_sampler(
#'   ll_fn,
#'   prior,
#'   nlive = 100,
#'   sampler = unif_cube()
#' )
#' unit_sampler
ernest_sampler <- function(
  log_lik,
  prior,
  sampler = rwmh_cube(),
  nlive = 500,
  first_update = NULL,
  update_interval = NULL,
  seed = NULL
) {
  if (!inherits(log_lik, "ernest_likelihood")) {
    log_lik <- create_likelihood(fn = log_lik)
  }
  obj <- new_ernest_sampler(
    log_lik_fn = log_lik,
    prior = prior,
    lrps = sampler,
    nlive = nlive,
    first_update = first_update %||% as.integer(nlive * 2.5),
    update_interval = update_interval %||% as.integer(nlive * 1.5),
    seed = seed
  )

  try_fetch(
    {
      compile(obj)
      obj$run_env <- new_environment()
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
