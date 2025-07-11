#' Prepare an `ernest_sampler` object for nested sampling
#'
#' Create a new set of live points/particles for a new nested sampling run, or
#' check the current state of the live points before continuing a previous run.
#'
#' @param object An object of class `ernest_sampler`.
#' @inheritParams rlang::args_dots_empty
#' @param clear A logical value indicating whether to reload `object` before
#' creating new live points. If `TRUE`, former results from the sampler are
#' removed from the object. If `FALSE`, the sampler will not drop prior results,
#' and continue using live points from the last sampling iteration performed.
#'
#' @details
#' `compile` checks whether the live points within a nested sampler are
#' of the expected structure (e.g., the matrix contains the proper dimensions,
#' every live point is described with finite values, and likelihood values are
#' either finite or `-Inf`). If these checks fail, or if `clear = TRUE`, the
#' sampler is reset and a new set of live points is generated.
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
compile.ernest_sampler <- function(object, ..., clear = FALSE) {
  check_dots_empty()
  object$compile(clear)
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
      cli::cli_abort(
        "Can't create live points.",
        parent = cnd
      )
    }
  )
}

#' Validate an existing nested sample for correctness.
#'
#' @param live A list containing live points (`unit`, `point`, `log_lik`).
#' @param n_points The expected number of live points.
#' @param n_var The expected number of dimensions for each point.
#' @param call The calling environment for error handling.
#'
#' @return Throws an error or warning if validation fails; otherwise, returns
#' NULL.
#' @noRd
check_live <- function(unit, log_lik, n_points, n_var, call = caller_env()) {
  tmplate <- matrix(nrow = n_points, ncol = n_var)
  # unit: Must be a matrix of points with dim [n_points, n_var],
  # all points must be finite and within [0, 1]
  if (!is.matrix(unit)) {
    abort(
      "Unit points must be stored as a matrix.",
      call = call
    )
  }
  if (!identical(dim(unit), dim(tmplate))) {
    cli::cli_abort(
      "Unit points must be stored as a matrix with dim. `c({n_points}, {n_var})`.",
      call = call
    )
  }
  if (any(!is.finite(unit))) {
    abort("Unit points must contain only finite values.", call = call)
  }
  if (any(unit < 0) || any(unit > 1)) {
    abort("Unit points must contain values within [0, 1].", call = call)
  }
  # loglik: Must be numeric vector of length n_points. Must contain only values
  # that are either finite or -Inf. Abort if log_lik contains no
  # unique values, warn if 10% of the values are duplicates.
  vctrs::vec_check_size(log_lik, size = n_points, call = caller_env)
  idx <- intersect(which(!is.finite(log_lik)), which(log_lik != -Inf))
  if (length(idx) > 0L) {
    len <- length(idx)
    cli::cli_abort(c(
      "Couldn't avoid calculating non-finite log-likelihood values.",
      "i" = "Log-likelihood values can only be finite or `-Inf`.",
      "x" = "There {?is/are} {len} non-finite, non-`-Inf` value{?s}."
    ), call = call)
  }
  idx <- which(log_lik == -Inf)
  if (length(idx) > 0L) {
    len <- length(idx)
    cli::cli_warn(
      "Found {len} log-likelihood value{?s} equal to `-Inf`.", call = call
    )
  }
  unique_logl <- unique(log_lik)
  if (length(unique_logl) == 1) {
    cli::cli_abort(c(
      "Couldn't generate unique log-likelihood values for each point.",
      "x" = "Every point had a calculated log-lik. value of {unique_logl}.",
      "i" = "This generally indicates an error within a log. lik. function."
    ), call = call)
  }
  if (length(unique_logl) < length(log_lik) * 0.25) {
    perc <- prettyNum(length(unique_logl) / length(log_lik))
    cli::cli_warn(c(
      "Suspected flatness in the log-likelihood surface.",
      "x" = "Only {perc}% of the live points have unique log-lik. values.",
      "i" = "Consider reviewing your model or adjusting your prior."
    ), call = call)
  }
  NULL
}
