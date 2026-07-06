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
#' @seealso [generate.ernest_run()]
#'
#' @examples
#' prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- ernest_sampler(ll_fn, prior, nlive = 100)
#'
#' # Compile the sampler to add a live set
#' compile(sampler)
#' head(sampler$live_env$unit)
#'
#' # Continue a previous run
#' \donttest{
#' data(example_run)
#' sampler_2 <- compile(example_run)
#' sampler_2
#' }
#'
#' # Make a new sampler from a previous run
#' sampler_3 <- compile(example_run, clear = TRUE)
#' sampler_3
#' @rdname compile-ernest
#' @export
compile.ernest_sampler <- function(object, ...) {
  preserve_seed(attr(object, "seed"))
  live <- new_live_set(object$lrps, object$nlive)
  write_live_set(live, object)
  object
}

#' @rdname compile-ernest
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
      refresh_frac = object$refresh_frac,
      seed = attr(object, "seed")
    )
    object <- do.call(new_ernest_sampler, elem)
    return(NextMethod())
  }
  preserve_seed(attr(object, "seed"))

  # Fill live set
  prev <- object$rcrd
  live <- get_live_set(prev, object$nlive)
  write_live_set(live, object)
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
new_live_set <- function(lrps, nlive, call = caller_env()) {
  try_fetch(
    {
      unit <- matrix(
        stats::runif(nlive * lrps$nvar),
        ncol = lrps$nvar
      )
      log_lik <- lrps$unit_log_fn(unit)
      vctrs::df_list(
        "unit" = unit,
        "log_lik" = log_lik,
        "birth_lik" = -Inf,
        "id" = as.character(seq_len(nlive))
      )
    },
    error = function(cnd) {
      cli::cli_abort(
        "Error when creating the live set.",
        parent = cnd,
        call = call
      )
    }
  )
}

#' Validate a live set for correctness
#'
#' @param live A list containing the `unit`, `log_lik`, and `birth_lik`
#' components of the live set.
#' @param object The `ernest_sampler` object undergoing validation.
#' @param call The calling environment for error handling.
#'
#' @return Returns the live_env bound to object, with the list entrants bound
#' for nested sampling.
#' @importFrom vctrs vec_cast
#' @noRd
write_live_set <- function(live, object, call = caller_env()) {
  nlive <- object$nlive
  nvar <- object$lrps$nvar

  # Prototype Checks
  unit <- vec_cast(
    live$unit,
    to = matrix(double(), ncol = nvar),
    to_arg = "matrix(ncol = object$lrps$nvar)",
    call = call
  )
  log_lik <- vec_cast(live$log_lik, to = double(), call = call)
  birth_lik <- vec_cast(live$birth_lik, to = double(), call = call)
  id <- vec_cast(live$id, to = character(), call = call)

  # Size Checks
  vctrs::list_check_all_size(
    list(
      "unit" = unit,
      "log_lik" = log_lik,
      "birth_lik" = birth_lik,
      "id" = id
    ),
    size = nlive,
    allow_null = FALSE,
    arg = "live",
    call = call
  )

  # Bounds Checking
  if (any(!is.finite(unit)) || min(unit) < 0 || max(unit) > 1) {
    cli::cli_abort(
      "`unit` must contain only finite values between 0 and 1.",
      call = call
    )
  }
  if (any(is.na(log_lik) | is.nan(log_lik) | log_lik == Inf)) {
    cli::cli_abort(
      "`log_lik` must contain only finite values or `-Inf`.",
      call = call
    )
  }
  if (any(is.na(birth_lik) | is.nan(birth_lik) | birth_lik == Inf)) {
    cli::cli_abort(
      "`birth_lik` must contain only finite values or `-Inf`.",
      call = call
    )
  }
  if (vctrs::vec_duplicate_any(id)) {
    cli::cli_abort(
      "`id` must contain unique values.",
      call = call
    )
  }

  # Plateau Checks
  n_unique <- vctrs::vec_unique_count(log_lik)
  if (n_unique == 1L && nlive > 1L) {
    cli::cli_abort(
      c(
        "`log_lik` must contain a range of likelihood values.",
        "x" = "`log_lik` currently contains one unique value ({log_lik[1]})."
      ),
      call = call
    )
  } else if (n_unique < (nlive * 0.75)) {
    cli::cli_warn(
      c(
        "`log_lik` may contain a likelihood plateau; proceed with caution.",
        "!" = "Only {n_unique}/{nlive} likelihood values are unique."
      ),
      call = call
    )
  }

  env_bind(
    object$live_env,
    "unit" = unit,
    "log_lik" = log_lik,
    "birth_lik" = birth_lik,
    "id" = id
  )
  object$live_env
}
