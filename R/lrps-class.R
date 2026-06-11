#' Create a new likelihood-restricted prior sampler (LRPS)
#'
#' Constructs an LRPS, accepted by [ernest_sampler()] and used to create new
#' points during a nested sampling run.
#'
#' @param unit_log_fn `[function]`\cr Takes a matrix of points in the unit
#' cube and returns a numeric vector of log-likelihood values. Optional, and
#' updated when called by `ernest_sampler()`.
#' @param nvar `[integer(1)]`\cr Number of dimensions within the prior space.
#' Optional, and updated when called by `ernest_sampler()`.
#' @param max_loop `[integer(1)]`\cr Maximum number of attempts to generate
#' points. Inferred from the `ernest.max_loop` option.
#' @param batch_size `[integer(1)]`\cr Number of proposals to generate per
#' batch for region-based LRPS methods. Inferred from the `ernest.batch_size`
#' option.
#' @param cache `[environment]` Environment for caching information required for
#' the specific LRPS method. Created if left `NULL`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Name-value pairs for
#' additional elements for `ernest_lrps'` subclasses
#' @param .class `[character()]`\cr Names for the subclass of `ernest_lrps`.
#' @inheritParams rlang::check_exclusive
#'
#' @srrstats {G2.4, G2.4a, G2.4b} Explicit conversion of inputs to expected
#' types or error messages.
#'
#' @returns `[ernest_lrps]`, a named list containing the arguments provided.
#'
#' @details
#' Nested sampling relies on generating a series of points in the prior space
#' with increasing log-likelihood values. This is accomplished using a
#' likelihood-restricted prior sampler (LRPS), which generates independent and
#' identically distributed points from the prior, subject to a hard likelihood
#' constraint.
#'
#' To create your own LRPS, subclass `new_ernest_lrps` and provide S3 methods
#' for [propose()] and [update_lrps()] for your subclass.
#'
#' @section Loop safety:
#' An LRPS may fail to produce a point satisfying the likelihood constraint
#' (for example, due to rounding errors or poor parameterisation). To avoid
#' infinite loops during a nested sampling run, `propose()` will terminate a
#' run if it cannot find a likelihood-restricted point after a maximum number of
#' calls to the likelihood function. Adjust this limit with the
#' `ernest.max_loop` option (default: 1e6).
#'
#' @aliases ernest_lrps
#' @keywords internal
#' @export
new_ernest_lrps <- function(
  unit_log_fn = NULL,
  nvar = NULL,
  max_loop = getOption("ernest.max_loop", 1e6L),
  batch_size = getOption("ernest.batch_size", 100L),
  cache = NULL,
  ...,
  .class = NULL,
  .call = caller_env()
) {
  check_dots_used(env = .call, call = .call)
  check_function(unit_log_fn, allow_null = TRUE, call = .call)
  check_number_whole(nvar, min = 1, allow_null = TRUE, call = .call)
  check_number_whole(
    max_loop,
    min = 1,
    allow_infinite = FALSE,
    arg = "getOption('ernest.max_loop')",
    call = .call
  )
  check_number_whole(
    batch_size,
    min = 1,
    allow_infinite = FALSE,
    arg = "getOption('ernest.batch_size')",
    call = .call
  )
  check_environment(cache, allow_null = TRUE, call = .call)

  elem <- list(
    unit_log_fn = unit_log_fn,
    nvar = if (is.null(nvar)) NULL else as.integer(nvar),
    max_loop = as.integer(max_loop),
    batch_size = as.integer(batch_size),
    cache = cache %||% new_environment()
  )
  env_poke(elem$cache, "neval", 0L)

  new_elem <- c(elem, list2(...))
  vctrs::vec_names2(new_elem, repair = "check_unique")
  structure(
    new_elem,
    class = c(.class, "ernest_lrps")
  )
}

#' @noRd
#' @export
format.ernest_lrps <- function(x, ...) "Abstract"

#' @noRd
#' @export
print.ernest_lrps <- function(x, ...) {
  cli::cli_text("{format(x)}:")

  cli::cli_par()
  cli::cli_text("# Dimensions: {x$nvar %||% 'Uninitialized'}")
  cli::cli_text("# Calls since last update: {env_get(x$cache, 'neval', 0)}")
  cli::cli_end()
  invisible(x)
}

#' Generate a new point using LRPS
#'
#' Use an LRPS method to replace points within the live set during a nested
#' sampling run. This method must be implemented on all classes that inherit
#' from `ernest_lrps`.
#'
#' @param x [[ernest_lrps]]\cr A likelihood-restricted prior sampler.
#' @param original `[double(x$nvar)]`\cr A parameter vector which can be used
#' to start the proposal process. Optional; if `NULL` proposals are generated
#' from sampling the unconstrained [unit cube][unif_cube()].
#' @param criterion `[double(1)]`\cr A log-likelihood value that restricts
#' the region of prior space to sample from.
#'
#' @returns `[list]`, containing at least these named elements:
#'
#' * `unit`: `[double(x$nvar)]` The replacement point, expressed in the
#' unit-cube.
#' * `log_lik`: `[double(1)]` The log-likelihood value at `unit`.
#' * `neval`: `[integer(1)]` The number of evaluations of `unit_log_fn` needed
#' to generate `unit`. Do not confuse this with the number of samples evaluated
#' during the call to `propose`, as `unit_log_fn` may be capable of evaluating
#' multiple parameters with a single call.
#'
#' @keywords internal
#' @export
propose <- function(x, original = NULL, criterion = -Inf) {
  UseMethod("propose")
}

#' @noRd
#' @export
propose.ernest_lrps <- function(
  x,
  original = NULL,
  criterion = -Inf
) {
  if (!is.null(original)) {
    cli::cli_abort("`x` must not be the abstract class {.cls ernest_lrps}.")
  }
  propose_cube(
    unit_log_fn = x$unit_log_fn,
    criterion = criterion,
    nvar = x$nvar,
    batch_size = x$batch_size,
    max_loop = x$max_loop,
    cache = x$cache
  )
}

#' Generate a new point in a unit cube
#'
#' @param unit_log_fn Function to compute log-likelihood in unit space.
#' @param criterion Double scalar. A log-likelihood value that proposed points
#' must satisfy.
#' @param nvar Number of dimensions.
#' @param batch_size Number of proposals to generate per batch.
#' @param max_loop Maximum number of attempts to generate a point.
#' @param cache Environment for caching points run in batches.
#'
#' @returns A list with:
#' * `unit`: Vector of proposed points in the prior space.
#' * `log_lik`: Numeric vector of log-likelihood values for the proposed.
#' * `neval`: Number of likelihood evaluations made to generate the proposed
#' point.
#' @noRd
propose_cube <- function(
  unit_log_fn,
  criterion,
  nvar,
  batch_size,
  max_loop,
  cache
) {
  batch <- matrix(NaN, nrow = batch_size, ncol = nvar)
  log_lik <- double(batch_size)
  for (i in seq_len(max_loop)) {
    batch[,] <- stats::runif(batch_size * nvar)
    log_lik[] <- NaN
    log_lik <- unit_log_fn(batch)
    accepted <- match(TRUE, log_lik >= criterion)
    if (!is.na(accepted)) {
      return(list(
        unit = batch[accepted, , drop = TRUE],
        log_lik = log_lik[[accepted]],
        neval = i
      ))
    }
  }
  list(unit = NULL, log_lik = NULL, neval = max_loop)
}

#' Update an LRPS
#'
#' Update the behaviour of a likelihood-restricted prior sampler with interim
#' information from a nested sampling run.
#'
#' @param x [[ernest_lrps]]\cr A likelihood-restricted prior sampler.
#' @param unit `[matrix(double(), integer(), x$nvar)]`\cr The current live set
#' stored within the run. Optional; if NULL no LRPS updates based on the state
#' of the live set will be made.
#' @param log_volume `[double(1)]`\cr The current log-volume of the nested
#' sampling run.
#' @param ... Presently ignored.
#'
#' @returns [[ernest_lrps]], created by reconstructing `x` with updated
#' parameters.
#'
#' @details
#' During a nested sampling run, you may wish to update the internal parameters
#' of the LRPS based on sampler performance or other criterion. The frequency of
#' these updates is set by the `first_update` and `update_interval` arguments of
#' [ernest_sampler()].
#'
#' If you are creating your own [ernest_lrps] subclass, implement this method to
#' specify any special update behaviour. The default method reconstructs the
#' LRPS with current parameters and resets the likelihood call counter in the
#' cache.
#'
#' @keywords internal
#' @export
update_lrps <- function(x, ...) {
  UseMethod("update_lrps")
}

#' @noRd
#' @export
update_lrps.ernest_lrps <- function(x, unit = NULL, ...) {
  do.call(new_ernest_lrps, as.list(x))
}
