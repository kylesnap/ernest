#' Prepare an `ernest_sampler` object for nested sampling
#'
#' Create a new set of live points/particles for a new nested sampling run, or
#' check the current state of the live points before continuing a previous run.
#'
#' @param object (ernest_sampler) An object of class `ernest_sampler`.
#' @inheritParams rlang::args_dots_empty
#' @param seed (integer or NA, optional) Specification for the random number
#' generator.
#' * integer: Passed to [set.seed()].
#' * `NULL`: Passed to [set.seed()], which reinitializes the generator as if
#' no seed has yet been set.
#' * `NA`: Make no changes to the current seed if set. If `compile` has been
#' called on `object` before, then `NA` will ensure that the seed remain
#' identical between runs.
#' @param clear (boolean) Whether to reset the sampler before compiling.
#' * `TRUE`: Previous results stored in `object` are removed, and live points
#' are generated and validated.
#' * `FALSE`: Previous results stored in `object` are retained, and live points
#' are validated.
#'
#' @details
#' The `compile` function prepares an `ernest_sampler` object for nested
#' sampling by ensuring that its set of live points is valid and ready for use.
#' In addition to constructing the live point set for new runs or when
#' `clear = FALSE`, compile also ensures that:
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
#' Random number generation can be seeded either through the `seed` argument,
#' or by calling [set.seed()] before running `compile` or `generate`.
#'
#' @returns `object`, invisibly.
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
#' @method compile ernest_sampler
#' @export
compile.ernest_sampler <- function(object, ..., seed = NA, clear = FALSE) {
  check_dots_empty()
  object$compile(seed = seed, clear = clear)
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
        parent = cnd
      )
    }
  )
}

#' Validate an existing nested sample for correctness.
#'
#' @param unit A numeric matrix of live points in the unit hypercube, with shape
#' (n_points, n_var).
#' @param log_lik A numeric vector of log-likelihood values, one for each live
#' point (length n_points).
#' @param n_points Integer. The expected number of live points (rows in `unit`,
#' length of `log_lik`).
#' @param n_var Integer. The expected number of dimensions for each point
#' (columns in `unit`).
#' @param call The calling environment for error handling (for advanced use).
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
check_live <- function(unit, log_lik, n_points, n_var, call = caller_env()) {
  # Check live point matrix using checkmate.
  msg <- checkmate::check_matrix(
    unit,
    mode = "numeric",
    nrows = n_points,
    ncols = n_var,
    any.missing = FALSE,
    all.missing = FALSE
  )
  if (!isTRUE(msg)) {
    format_checkmate(msg, "unit", call = call)
  }
  msg <- checkmate::check_numeric(
    unit,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    all.missing = FALSE,
    finite = TRUE
  )
  if (!isTRUE(msg)) {
    format_checkmate(msg, "unit", call = call)
  }

  msg <- checkmate::check_numeric(
    log_lik,
    len = n_points,
    any.missing = FALSE,
    all.missing = FALSE
  )
  if (!isTRUE(msg)) {
    format_checkmate(msg, "log_lik", call = call)
  }
  if (any(!is.finite(log_lik) & log_lik != -Inf)) {
    nonfinite <- unique(log_lik[!is.finite(log_lik) & log_lik != -Inf])
    cli_abort(
      "`{log_lik}` must contain only finite values or -Inf, not {nonfinite}.",
      call = call
    )
  }

  n_unique <- vctrs::vec_unique_count(log_lik)
  if (n_unique == 1L) {
    cli_abort(
      c(
        "Log likelihoods of the live points must not be a plateau.",
        "!" = "Log likelihood of all {n_points} points = {log_lik[1]}."
      ),
      call = call
    )
  }
  if (n_unique < (n_points * 0.75)) {
    cli_warn(
      c(
        "Potential likelihood plateau; proceed with caution.",
        "!" = "{n_unique} unique likelihoods across {n_points} live points."
      ),
      call = call
    )
  }

  NULL
}
