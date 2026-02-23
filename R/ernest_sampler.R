#' Prepare a new nested sampling run
#'
#' Initializes an `ernest_sampler` object containing the components required to
#' perform nested sampling. This object can then be used to build sequences of
#' nested samples with [generate()].
#'
#' @param log_lik `[function]` or [[ernest_likelihood]]\cr
#' A function which computes the log-likelihood of a given model. If a
#' function, it is wrapped with [create_likelihood()].
#' @param prior [[ernest_prior]]\cr Describes the prior space
#' within which to generate samples.
#' @param sampler [[ernest_lrps]]\cr Specifies the
#' likelihood-restricted prior sampling method used to replace points
#' within the live set.
#' @param nlive `[integer(1)]`\cr The number of points to generate
#' within the live set. Strictly positive.
#' @param first_update `[integer(1)]`\cr The number of likelihood
#' calls to make with the default [uniform LRPS][unif_cube()] method before
#' swapping to the technique described by `sampler`. Optional; if left `NULL`
#' this is set to `nlive * 2.5`.
#' @param update_interval `[integer(1)]`\cr The number of likelihood
#' calls between updates to the `sampler` object. Optional; if left `NULL`
#' this is set to `nlive * 1.5`.
#' @param seed `[integer(1)]`\cr Sets the random seed controlling the
#' random number generator for nested sampling runs. Optional; if left `NA`
#' the [.Random.seed] set within R is preserved and restored after a run.
#'
#' @returns `[ernest_sampler]`\cr
#' A named list, containing a specification for a nested sampling run. Contains
#' the arguments passed to this function as well as an environment `run_env`,
#' which is used to store the live set during sampling.
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
#' @export
ernest_sampler <- function(
  log_lik,
  prior,
  sampler = rwmh_cube(),
  nlive = 500,
  first_update = NULL,
  update_interval = NULL,
  seed = NA
) {
  if (!inherits(log_lik, "ernest_likelihood")) {
    log_lik <- create_likelihood(log_lik)
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
