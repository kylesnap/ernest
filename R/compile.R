#' Compile an `ernest_sampler` object.
#'
#' Prepare the `ernest_sampler` for generating nested samples by validating
#' the set of live points within the object, creating live points if none
#' exist yet.
#'
#' Usually, a user will not need to call this method directly, as it is
#' called automatically by [generate()]. `compile()` is kept as a distinct step
#' from [generate()] so users can understand and troubleshoot errors with
#' their log-likelihood and prior transformation functions before a run begins.
#' It also catches errors that were generated during a run: These are more indicative
#' of a problem with ernest itself, and such errors are labelled appropriately.
#'
#' @param object An `ernest_sampler` object.
#' @param refresh Whether to clear existing points from `object` and generating new ones.
#' If `TRUE`, the function will clear both the live points and dead points gathered
#' from previous runs.
#' @param ... Ignored.
#'
#' @returns `object`, invisibly.
compile.ernest_sampler <- function(object, refresh = FALSE, ...) {
  object$compile(refresh)
}

#' Internal method for creating a live sample with `n` live points
#' @noRd
create_live <- function(lrps, n_points, call = caller_env()) {
  units <- matrix(
    stats::runif(n_points * lrps$n_dim),
    nrow = n_points,
    ncol = lrps$n_dim
  )
  rlang::try_fetch(
    {
      points <- t(apply(units, 1, lrps$prior_transform))
      if (!all(is.finite(points))) {
        rlang::abort("`prior_transform` produced non-finite values.")
    }
    },
    error = \(cmd) {
      cli::cli_abort("Error when generating live points:", parent = cmd, call = call)
    }
  )
  rlang::try_fetch(
    {
      log_lik <- apply(points, 1, lrps$log_lik)
      if (!is_double(log_lik, n = n_points)) {
        cli::cli_abort("`log_lik` must be a vector of length {n_points}.")
      }
      if (any(is.nan(log_lik) | is.na(log_lik))) {
        rlang::abort("`log_lik` produced non-finite values.")
      }
      if (any(is.infinite(log_lik))) {
        if (any(log_lik == Inf)) {
          rlang::abort("`log_lik` produced infinite values.")
        } else {
          rlang::warn("`log_lik` produced -Inf values.")
        }
      }
    },
    error = \(cmd) {
      cli::cli_abort("Error when evaluating `log_lik`:", parent = cmd, call = call)
    },
    warning = \(cmd) {
      cli::cli_warn("Warning when evaluating `log_lik`:", parent = cmd, call = call)
    }
  )
  list(
    "units" = units,
    "points" = points,
    "log_lik" = log_lik
  )
}

#' Internal method for validating an existing nested sample.
#'
#' Checks the following list of fatal conditions for the nested sampler:
#' * The live points are a list.
#' * The live points contain the following elements with corresponding types:
#'  * `units`: A matrix of values between [0, 1].
#'  * `points`: A matrix of finite values.
#'  * `log_lik`: A vector of finite or -Inf values.
#' * The number of live points matches the expected number of live points.
#' * The number of dimensions in the live points matches the expected number (n_points)
#' of dimensions (lrps)
#' * `units` and `points` are of the exact same dimensions.
#' * `points` has the same number of rows as `log_lik` has elements.
#'
#' @returns Depends on the state of live:
#' * If live is a list with correct structure, it will return NULL.
#' * If live is a list with incorrect structure, it will return an error prompting
#' the user to reconstruct the live points from scratch.
#' @noRd
check_live <- function(live, lrps, n_points) {
  if (is_empty(live)) {
    cli::cli_warn("No live points found.")
  }
  if (!is.list(live)) {
    cli::cli_abort("`live` must be a list.")
  }
  if (!all(c("units", "points", "log_lik") %in% names(live))) {
    cli::cli_abort("`live` must contain elements `units`, `points`, and `log_lik`.")
  }
  if (!is.matrix(live$units)) {
    cli::cli_abort("`live$units` must be a matrix.")
  }
  if (!is.matrix(live$points)) {
    cli::cli_abort("`live$points` must be a matrix.")
  }
  if (!is_double(live$log_lik, n = n_points)) {
    cli::cli_abort("`live$log_lik` must be a vector of doubles with length {n_points}.")
  }
  if (!identical(dim(live$units), as.integer(c(n_points, lrps$n_dim)))) {
    cli::cli_abort("`live$units` must be of dimension ({n_points}, {lrps$n_dim}).")
  }
  if (!identical(dim(live$units), dim(live$points))) {
    rlang::abort("`live$units` must have the same dimensions as `live$points`.")
  }
  if (any(live$units < 0 | live$units > 1)) {
    rlang::abort("`live$units` contains values outside of [0, 1].")
  }
  if (any(!is.finite(live$points))) {
    rlang::abort("`live$points` contains non-finite values.")
  }
  if (any(is.nan(live$log_lik) | is.na(live$log_lik))) {
    rlang::abort("`live$log_lik` contains non-finite values.")
  }
  if (any(live$log_lik == Inf)) {
    rlang::abort("`live$log_lik` contains `+Inf` values.")
  }
  NULL
}
