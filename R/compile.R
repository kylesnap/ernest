#' Compile the live set of points for nested sampling
#'
#' Prepares an object for nested sampling by validating and (re)generating its
#' live set. This ensures the sampler is viable before new points are drawn
#' during the nested sampling algorithm.
#'
#' @param object [`[ernest_run]`][ernest_run] or
#' [`[ernest_sampler]`][ernest_sampler]\cr Results from a nested sampling run.
#' @inheritParams rlang::args_dots_empty
#'
#' @details
#' `compile()` validates the live set bound to `object`, ensuring that:
#'
#' * Each point in the set is within the unit hypercube.
#' * The likelihood function returns valid values (finite double or `-Inf`) for
#'   each point.
#' * The live set does not represent a perfect likelihood plateau (i.e.,
#'   that all points share the same likelihood). A warning is issued if more
#'   than 25% of points share the same likelihood value.
#'
#' If validation fails, the live set is removed from `object`, preventing
#' further sampling until the issue is resolved.
#'
#' @returns A copy of `[object]`.
#'
#' The copy is guaranteed to have a valid live set, created according to the
#' class of `object` and the value of `clear`:
#' * If `object` is an `ernest_sampler`, or if `clear = TRUE`, a new live set is
#' created from scratch.
#' * If `object` is an `ernest_run`, the live set is regenerated from previous
#' results.
#'
#' @seealso
#' * [generate()] for running samplers with compiled live sets.
#'
#' @examples
#' prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- ernest_sampler(ll_fn, prior, nlive = 100)
#'
#' # Compile the sampler to add a live set
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
#' @rdname compile.ernest_run
#' @export
compile.ernest_sampler <- function(object, ...) {
  preserve_seed(object)
  check_dots_empty()
  object <- refresh_ernest_sampler(object)

  # Fill live set
  live <- create_live(object$lrps, object$nlive)
  env_poke(object$run_env, "unit", live$unit, create = TRUE)
  env_poke(object$run_env, "log_lik", live$log_lik, create = TRUE)
  env_poke(
    object$run_env,
    "birth_lik",
    rep(-Inf, object$nlive),
    create = TRUE
  )

  check_live_set(object)
  object
}

#' @rdname compile.ernest_run
#'
#' @param clear `[logical(1)]`\cr If `TRUE`, clears results from previous runs
#' before compiling. If `FALSE`, retains previous results and validates the live
#' set.
#'
#' @export
compile.ernest_run <- function(
  object,
  clear = FALSE,
  ...
) {
  check_dots_empty()
  check_bool(clear)
  if (clear) {
    elem <- list(
      log_lik_fn = object$log_lik_fn,
      prior = object$prior,
      lrps = object$lrps,
      nlive = object$nlive,
      first_update = object$first_update,
      update_interval = object$update_interval,
      seed = attr(object, "seed")
    )
    object <- do.call(new_ernest_sampler, elem)
    return(compile(object, ...))
  }
  preserve_seed(object)

  # Fill live set
  live_positions <- vctrs::vec_as_location(
    seq(object$niter),
    vctrs::vec_size(object$weights$log_lik)
  )
  env_bind(
    object$run_env,
    unit = object$samples$unit_cube[-live_positions, ],
    log_lik = object$weights$log_lik[-live_positions],
    birth_lik = object$weights$birth_lik[-live_positions]
  )
  try_fetch(
    check_live_set(object),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Can't create live set from the previous run.",
          "i" = "Do you need to set `clear`?"
        ),
        parent = cnd
      )
    }
  )
  object
}

#' Create a live sample with `nlive` points
#'
#' @param lrps An object containing the likelihood-restricted prior sampler.
#' @param nlive The number of points to generate.
#' @param call The calling environment for error handling.
#'
#' @return A list containing `unit` and `log_lik` matrices or vectors.
#' @noRd
create_live <- function(lrps, nlive, call = caller_env()) {
  try_fetch(
    {
      live <- replicate(nlive, propose(lrps))
      live <- list(
        "unit" = do.call(rbind, live["unit", ]),
        "log_lik" = list_c(live["log_lik", ])
      )
      live
    },
    error = function(cnd) {
      cli::cli_abort(
        "Error when creating the live set.",
        parent = cnd,
        call = call
      )
    }
  )
  order_logl <- order(live$log_lik)
  list(
    "log_lik" = live$log_lik[order_logl],
    "unit" = live$unit[order_logl, , drop = FALSE]
  )
}

#' Validate a live set for correctness
#'
#' @param sampler The `ernest_sampler` object undergoing validation.
#' @param call The calling environment for error handling.
#'
#' @return Returns NULL invisibly if validation passes, otherwise throws an
#' error or warning.
#' @noRd
check_live_set <- function(sampler, call = caller_env()) {
  nlive <- sampler$nlive
  n_dim <- attr(sampler$prior, "n_dim")

  # Live Point Check
  unit <- env_get(sampler$run_env, "unit")
  check_matrix(
    unit,
    nrow = nlive,
    ncol = n_dim,
    lower = 0,
    upper = 1,
    arg = "unit",
    call = call
  )

  # Log Lik Checks
  log_lik <- env_get(sampler$run_env, "log_lik")
  if (!is_bare_double(log_lik, n = nlive)) {
    cli::cli_abort(
      "`log_lik` must be a double vector with length {nlive}.",
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
  if (n_unique < (nlive * 0.75)) {
    cli::cli_warn(
      c(
        "`log_lik` may contain a likelihood plateau; proceed with caution.",
        "!" = "Only {n_unique}/{nlive} likelihood values are unique."
      ),
      call = call
    )
  }

  # Birth vector
  birth_lik <- env_get(sampler$run_env, "birth_lik")
  if (!is_double(birth_lik)) {
    stop_input_type(birth_lik, "a double vector")
  }
  vctrs::vec_check_size(birth_lik, size = nlive, call = call)

  invisible(NULL)
}
