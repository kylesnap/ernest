#' Prepare an `ernest_sampler` object for nested sampling
#'
#' Create a new set of live points/particles for a new nested sampling run, or
#' check the current state of the live points before continuing a previous run.
#'
#' @param object An object of class `ernest_sampler`.
#' @inheritParams rlang::args_dots_empty
#' @param seed Either a single value, interpretted as an integer, or `NA` or
#' `NULL`.
#' * If an integer or NULL, `seed` is passed to [set.seed()] to set the state
#' of the random number generator. `NULL` reinitializes the generator as if
#' no seed has yet been set.
#' * If `NA`, the random number generator is set with the seed stored in the
#' `ernest_run` object bound to `object`. If this object does not exist (i.e.,
#' if not prior runs have been performed), the current state of the generator
#' is recorded and stored for future objects.
#' @param clear A logical value indicating whether to reload `object` before
#' creating new live points. If `TRUE`, former results from the sampler are
#' removed from the object. If `FALSE`, the sampler will not drop prior results,
#' and continue using live points from the last sampling iteration performed.
#'
#' @details
#' The `compile` function prepares an `ernest_sampler` object for nested
#' sampling by ensuring that its set of live points is valid and ready for use.
#' In addition to constructing the live point set for new runs, the live points
#' are validated for a number of conditions:
#'
#' * Ensures that the points are each represented within the unit hypercube.
#' * Ensures that the wrapped likelihood function `ernest_likelihood` has a
#' valid return value for each point (finite double or `-Inf`).
#' * Ensures that the likelihood isn't at a plateau, warning you if there are
#' duplicate likelihood values.
#'
#' If `compile` fails these validation steps, the function will fail and the
#' set of live points will be removed from `object` with an informative error
#' message encouraging you to run the sampler from scratch with `clear = TRUE`.
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
  rlang::try_fetch(
    lrps$propose_uniform(criteria = rep(-1e300, n_points)),
    error = function(cnd) {
      fun <- eval(cnd$call[[1]], envir = lrps)
      cnd$call[[1]] <- if (identical(fun, lrps$private$prior_fn)) {
        expr(prior$fn)
      } else if ((identical(fun, lrps$private$log_lik_fn))) {
        expr(log_lik)
      } else {
        cnd$call[[1]]
      }
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
  # Check live point matrix.
  if (!is.numeric(unit) || !is.matrix(unit)) {
    stop_input_type(unit, "a numeric matrix", call = call)
  }
  if (ncol(unit) != n_var || nrow(unit) != n_points) {
    cli::cli_abort(
      c(
        "Live points matrix must have dim. {n_points} x {n_var}.",
        "x" = "Points are currently {nrow(unit)} x {ncol(unit)}."
      ),
      call = call
    )
  }
  if (!all(is.finite(unit))) {
    cli::cli_abort(
      "Live points matrix must only contain finite values.",
      call = call
    )
  }
  if (any(unit < 0 | unit > 1)) {
    cli::cli_abort(
      "Live points matrix must only contain values between 0 and 1.",
      call = call
    )
  }

  if (!is_double(log_lik, n = n_points)) {
    cli::cli_abort(
      c(
        "`log_lik` must be a double vector of length {n_points}",
        "!" = "You provided {obj_type_friendly(log_lik)}."
      ),
      call = call
    )
  }
  if (any(!is.finite(log_lik) & log_lik != -Inf)) {
    n_nonfinite <- sum(!is.finite(log_lik) & log_lik != -Inf)
    cli::cli_abort(
      c(
        "Live points' log-likelihoods must be finite or -Inf.",
        "x" = "Found {n_nonfinite} non-finite values."
      ),
      call = call
    )
  }

  uniq_ll <- vctrs::vec_unique(log_lik)
  n_unique <- length(uniq_ll)
  if (n_unique == 1L) {
    cli::cli_abort(
      c(
        "Log likelihoods of the live points must not be a plateau.",
        "x" = "Log likelihood of all {n_points} points = {uniq_ll}."
      ),
      call = call
    )
  }
  if (n_unique < n_points) {
    rep_ll <- vctrs::vec_unrep(log_lik)
    names <- as.character(rep_ll$key)[rep_ll$times != 1]
    vals <- paste0(
      as.character(rep_ll$times[rep_ll$times != 1]),
      " times"
    )
    names(vals) <- names
    cli::cli_warn(
      "{n_unique} of {n_points} likelihood values in the live set are unique.",
      call = call
    )
    cli::cli_alert_warning("Duplicated Values:")
    cli::cli_dl(vals)
  }

  NULL
}
