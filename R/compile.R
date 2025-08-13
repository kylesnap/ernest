#' Prepare an `ernest_sampler` object for nested sampling
#'
#' Create a new set of live points/particles for a new nested sampling run, or
#' check the current state of the live points before continuing a previous run.
#'
#' @param object (ernest_sampler or ernest_run) An object of class
#' `ernest_sampler` or `ernest_run`.
#' @inheritParams rlang::args_dots_empty
#' @param seed (integer or NA, optional) Specification for the random number
#' generator.
#' * integer and NULL: Passed to [set.seed()]. If `NULL`, this reinitializes the
#' generator as if no seed has yet been set.
#' * `NA`: Make no changes to the current seed. If `compile` has been
#' called on `object` before, then `NA` will ensure that the seed remain
#' identical between runs.
#' @param clear (boolean) Whether to clear results from previous runs before
#' compiling.
#' * `TRUE`: Previous results stored in `object` are removed, and live points
#' are generated and validated.
#' * `FALSE`: Previous results stored in `object` are retained, and live points
#' are validated.
#'
#' @details
#' The `compile` function prepares an `ernest_sampler` object for nested
#' sampling by ensuring that its set of live points is valid and ready for use.
#' In addition to constructing the live point set for runs, compile also ensures
#' that:
#'
#' * The live points are each represented within the unit hypercube.
#' * The wrapped likelihood function `ernest_likelihood` has a valid return
#' value for each point (either a finite double or `-Inf`).
#' * The live points don't represent a perfect plateau (i.e., all points share
#' the same likelihood). You are warned if more than 25% of the points
#' share the same likelihood value.
#'
#' If `compile` fails these validation steps, the set of live points will be
#' removed from `object`, preventing you from calling [generate()] on a
#' malformed sampler.
#'
#' @returns `object`, with a valid set of live points stored in the
#' `live_points` environment.
#' @examples
#' prior <- create_uniform_prior(n_dim = 2, lower = -1, upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- nested_sampling(ll_fn, prior, n_point = 100)
#'
#' # Add live points to the sampler
#' compile(sampler)
#' head(sampler$live_points$unit)
#'
#' # Check the status of the sampler with `clear = FALSE`
#' compile(sampler, clear = FALSE)
#' head(sampler$live_points$unit)
#'
#' # Reset the sampler with new live points with `clear = TRUE`
#' compile(sampler, clear = TRUE)
#' head(sampler$live_points$unit)
#' @export
compile.ernest_sampler <- function(object, ..., seed = NA) {
  check_dots_empty()
  object <- refresh_ernest_sampler(object)

  # Set seed
  if (is.null(seed) || !is.na(seed)) {
    check_number_whole(seed, allow_null = TRUE)
    env_poke(object$live_points, "seed", seed, create = TRUE)
  }
  set.seed(env_cache(object$live_points, "seed", NULL))

  # Fill live points
  cli::cli_inform("Creating new live points.")
  live <- create_live(object$lrps, object$n_points)
  env_poke(object$live_points, "unit", live$unit, create = TRUE)
  env_poke(object$live_points, "log_lik", live$log_lik, create = TRUE)
  env_poke(object$live_points, "birth", rep(0L, object$n_points))

  check_live_set(object)
  object
}

#' @rdname compile.ernest_sampler
#' @export
compile.ernest_run <- function(object, ..., seed = NA, clear = FALSE) {
  check_dots_empty()
  check_bool(clear)

  if (clear) {
    NextMethod(object)
  }

  # Fill live points
  live_positions <- vctrs::vec_as_location(
    seq(object$n_iter),
    vctrs::vec_size(object$log_lik)
  )
  env_bind(
    object$live_points,
    unit = object$samples_unit[-live_positions, ],
    log_lik = object$log_lik[-live_positions],
    birth = object$birth[-live_positions]
  )
  try_fetch(
    check_live_set(object),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Can't create live points from the previous run.",
          "i" = "Do you need to set `clear`?"
        ),
        parent = cnd
      )
    }
  )
  object
}

#' Create a live sample with `n` live points.
#'
#' @param lrps An object containing the likelihood-restricted prior sampler.
#' @param n_points The number of live points to generate.
#' @param call The calling environment for error handling.
#' @return A list containing `unit`, `point`, and `log_lik` matrices/vectors.
#' @noRd
create_live <- function(lrps, n_points, call = caller_env()) {
  try_fetch(
    lrps$propose_uniform(criteria = rep(-1e300, n_points)),
    error = function(cnd) {
      cli::cli_abort(
        "Can't create live points.",
        parent = cnd,
        call = call
      )
    }
  )
}

#' Validate an existing nested sample for correctness.
#'
#' @param sampler The ernest_sampler object undergoing validation.
#' @param call The calling environment for error handling.
#'
#' @srrstats {G2.13, G2.14, BS3.0} Before running, compile checks for uncaught
#' NA, NaN, or Inf values produced by the user inputs `log_lik` and `prior`.
#' @srrstats {G2.0, BS2.1, BS2.2, BS2.4} Compile checks that `prior`
#' creates parameters that are valid inputs to `log_lik`. Also warns when the
#' log-likelihood points surface contains flatness.
#'
#' @return Throws an error or warning if validation fails; otherwise, returns
#' NULL.
#' @noRd
check_live_set <- function(sampler, call = caller_env()) {
  n_points <- sampler$n_points
  n_dim <- sampler$prior$n_dim

  # Live Point Check
  unit <- env_get(sampler$live_points, "unit")
  check_matrix(
    unit,
    nrow = n_points,
    ncol = n_dim,
    lower = 0,
    upper = 1,
    arg = "unit",
    call = call
  )

  # Log Lik Checks
  log_lik <- env_get(sampler$live_points, "log_lik")
  check_double(log_lik, size = n_points, arg = "log_lik", call = call)

  n_unique <- vctrs::vec_unique_count(log_lik)
  if (n_unique == 1L) {
    cli::cli_abort(
      c(
        "`log_lik` must contain a range of likelihood values.",
        "x" = "`log_lik` currently contains one unique value ({log_lik[1]})."
      ),
      call = call
    )
  }
  if (n_unique < (n_points * 0.75)) {
    cli::cli_warn(
      c(
        "`log_lik` may contain a likelihood plateau; proceed with caution.",
        "!" = "Only {n_unique}/{n_points} likelihood values are unique."
      ),
      call = call
    )
  }

  # Birth vector
  birth <- env_get(sampler$live_points, "birth")
  if (!is_bare_integer(birth, n = n_points)) {
    cli::cli_abort(
      "`birth` vector cannot be missing from the `live_points` environment.",
      call = call
    )
  }

  invisible(NULL)
}
