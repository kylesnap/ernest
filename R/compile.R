#' Compile a set of live points for nested sampling
#'
#' Prepares an object for nested sampling by validating and (re)generating its
#' set of live points. This ensures the sampler is viable before new live points
#' are generated during the nested sampling algorithm.
#'
#' @param object An [ernest_sampler] or [ernest_run] object.
#'   * For `ernest_sampler`: Prepares a new sampler with a fresh set of live
#'     points.
#'   * For `ernest_run`: Regenerates live points from previous results, unless
#'     `clear = TRUE`.
#' @inheritParams rlang::args_dots_empty
#' @param seed An integer, `NULL`, or `NA`. Controls the random number
#' generator:
#'   * Integer or `NULL`: Passed to [set.seed()]. If `NULL`, reinitializes the
#'     generator as if no seed has yet been set.
#'   * `NA`: Makes no changes to the current seed. If `compile()` has been
#'     called on `object` before, `NA` ensures the seed remains identical
#'     between runs.
#' @param clear Logical. If `TRUE`, clears results from previous runs before
#' compiling. If `FALSE`, retains previous results and validates live points.
#'
#' @details `compile()` validates the set of live points in the sampler or run,
#' ensuring:
#'
#' * Each live point is within the unit hypercube.
#' * The likelihood function returns valid values (finite double or `-Inf`) for
#'   each point.
#' * The set of live points is not a perfect plateau (all points sharing the
#'   same likelihood). A warning is issued if more than 25% of points share the
#'   same likelihood value.
#'
#' If validation fails, the set of live points is removed, preventing further
#' sampling until the issue is resolved.
#'
#' @return A validated `object`, with a valid set of live points stored in its
#' `live_points` environment.
#'
#' @seealso
#' * [ernest_sampler()] for creating an `ernest_sampler` object.
#' * [generate()] for running nested sampling and details on the `ernest_run`
#'   object.
#'
#' @examples
#' prior <- create_uniform_prior(n_dim = 2, lower = -1, upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- ernest_sampler(ll_fn, prior, n_points = 100)
#'
#' # Compile the sampler to add live points
#' compile(sampler)
#' head(sampler$live_points$unit)
#'
#' # Continue a previous run
#' # run <- data(example_run)
#' # sampler_2 <- compile(example_run)
#' # sampler_2
#'
#' # Make a new sampler from a previous run
#' sampler_3 <- compile(example_run, clear = TRUE)
#' sampler_3
#' @rdname compile
#' @export
compile.ernest_sampler <- function(object, ..., seed = NA) {
  check_dots_empty()
  object <- refresh_ernest_sampler(object)
  set_ernest_seed(seed, object$live_points)

  # Fill live points
  cli::cli_inform("Creating new live points...")
  live <- create_live(object$lrps, object$n_points)
  env_poke(object$live_points, "unit", live$unit, create = TRUE)
  env_poke(object$live_points, "log_lik", live$log_lik, create = TRUE)
  env_poke(object$live_points, "birth", rep(0L, object$n_points))

  check_live_set(object)
  object
}

#' @rdname compile
#' @export
compile.ernest_run <- function(object, ..., seed = NA, clear = FALSE) {
  check_dots_empty()
  check_bool(clear)

  if (clear) {
    object <- list(
      log_lik_fn = object$log_lik_fn,
      prior = object$prior,
      lrps = object$lrps,
      n_points = object$n_points,
      first_update = object$first_update,
      update_interval = object$update_interval
    )
    return(compile.ernest_sampler(object, ..., seed = seed))
  }

  # Fill live points
  cli::cli_inform("Restoring live points from a previous run...")
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

#' Create a live sample with n_points live points
#'
#' @param lrps An object containing the likelihood-restricted prior sampler.
#' @param n_points Integer. The number of live points to generate.
#' @param call The calling environment for error handling.
#'
#' @return A list containing `unit`, `point`, and `log_lik` matrices or vectors.
#' @noRd
create_live <- function(lrps, n_points, call = caller_env()) {
  try_fetch(
    propose(lrps, criteria = rep(-1e300, n_points)),
    error = function(cnd) {
      cli::cli_abort(cnd$message, call = expr(compile()))
    },
    warning = function(cnd) {
      if (grepl("object length is not a multiple", cnd$message)) {
        cli::cli_abort(cnd$message, call = expr(compile()))
      } else {
        cli::cli_warn(cnd$message, call = expr(compile()))
      }
    }
  )
}

#' Validate a set of live points for correctness
#'
#' @param sampler The `ernest_sampler` object undergoing validation.
#' @param call The calling environment for error handling.
#'
#' @return Returns NULL invisibly if validation passes, otherwise throws an
#' error or warning.
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

#' Set a seed for nested sampling.
#'
#' @param seed An integer, `NULL`, or `NA`.
#' @param cache The cache from `object`, used to detect previous seeds.
#'
#' @returns NULL, invisibly.
#' @noRd
set_ernest_seed <- function(seed, cache, call = caller_env()) {
  if (is.null(seed) || !is.na(seed)) {
    check_number_whole(seed, allow_null = TRUE, call = call)
    set.seed(seed)
    env_poke(cache, "seed", .Random.seed)
  } else if (env_has(cache, "seed")) {
    .Random.seed <- env_get(cache, "seed")
    cli::cli_inform("Restored a previously saved RNG state.")
  } else {
    if (!exists(".Random.seed")) {
      set.seed(NULL)
    }
    env_poke(cache, "seed", .Random.seed)
  }
  invisible(NULL)
}
