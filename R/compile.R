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
#' @param seed `r lifecycle::badge("deprecated")` `seed` is no longer supported;
#' set RNG with the `seed` argument of [ernest_sampler()].
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
#' `run_env` environment.
#'
#' @seealso
#' * [ernest_sampler()] for creating an `ernest_sampler` object.
#' * [generate()] for running nested sampling and details on the `ernest_run`
#'   object.
#'
#' @examples
#' prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- ernest_sampler(ll_fn, prior, n_points = 100)
#'
#' # Compile the sampler to add live points
#' compile(sampler)
#' head(sampler$run_env$unit)
#'
#' # Continue a previous run
#' # run <- data(example_run)
#' # sampler_2 <- compile(example_run)
#' # sampler_2
#'
#' # Make a new sampler from a previous run
#' sampler_3 <- compile(example_run, clear = TRUE)
#' sampler_3
#' @importFrom lifecycle deprecated
#' @rdname compile
#' @export
compile.ernest_sampler <- function(object, ..., seed = deprecated()) {
  withr::local_seed(attr(object, "seed"))
  check_dots_empty()
  object <- refresh_ernest_sampler(object)
  if (lifecycle::is_present(seed)) {
    lifecycle::deprecate_warn(
      when = "1.1.0",
      what = "compile(seed)",
      details = "Use `ernest_sampler()`'s `seed` argument instead."
    )
  }

  # Fill live points
  live <- create_live(object$lrps, object$n_points)
  env_poke(object$run_env, "unit", live$unit, create = TRUE)
  env_poke(object$run_env, "log_lik", live$log_lik, create = TRUE)
  env_poke(object$run_env, "birth", rep(0L, object$n_points))

  check_live_set(object)
  alert_info("Created {object$n_points} live points.")
  object
}

#' @rdname compile
#' @export
compile.ernest_run <- function(
  object,
  ...,
  seed = deprecated(),
  clear = FALSE
) {
  withr::local_seed(attr(object, "seed"))
  check_dots_empty()
  check_bool(clear)
  if (lifecycle::is_present(seed)) {
    lifecycle::deprecate_warn(
      when = "1.1.0",
      what = "compile(seed)",
      details = "Use `ernest_sampler()`'s `seed` argument instead."
    )
  }

  if (clear) {
    object <- list(
      log_lik_fn = object$log_lik_fn,
      prior = object$prior,
      lrps = object$lrps,
      n_points = object$n_points,
      first_update = object$first_update,
      update_interval = object$update_interval
    )
    return(compile.ernest_sampler(object, ...))
  }

  # Fill live points
  live_positions <- vctrs::vec_as_location(
    seq(object$n_iter),
    vctrs::vec_size(object$log_lik)
  )
  env_bind(
    object$run_env,
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
  alert_info("Restored {object$n_points} live points from a previous run.")
  object
}

#' Create a live sample with n_points live points
#'
#' @param lrps An object containing the likelihood-restricted prior sampler.
#' @param n_points Integer. The number of live points to generate.
#' @param call The calling environment for error handling.
#'
#' @return A list containing `unit` and `log_lik` matrices or vectors.
#' @noRd
create_live <- function(lrps, n_points, call = caller_env()) {
  try_fetch(
    {
      live <- replicate(n_points, propose(lrps))
      live <- list(
        "unit" = do.call(rbind, live["unit", ]),
        "log_lik" = list_c(live["log_lik", ])
      )
      live
    },
    error = function(cnd) {
      cli::cli_abort(cnd$message, call = call)
    }
  )
  order_logl <- order(live$log_lik)
  list(
    "log_lik" = live$log_lik[order_logl],
    "unit" = live$unit[order_logl, , drop = FALSE]
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
  n_dim <- attr(sampler$prior, "n_dim")

  # Live Point Check
  unit <- env_get(sampler$run_env, "unit")
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
  log_lik <- env_get(sampler$run_env, "log_lik")
  if (!is_bare_double(log_lik, n = n_points)) {
    cli::cli_abort(
      "`log_lik` must be a double vector with length {n_points}.",
      call = call
    )
  }
  if (any(is.na(log_lik) | is.nan(log_lik) | log_lik == Inf)) {
    cli::cli_abort(
      "`log_lik` must contain only finite values or `-Inf`.",
      call = call
    )
  }

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
  birth <- env_get(sampler$run_env, "birth")
  if (!is_bare_integer(birth, n = n_points)) {
    cli::cli_abort(
      "`birth` vector cannot be missing from the `run_env` environment.",
      call = call
    )
  }

  invisible(NULL)
}
