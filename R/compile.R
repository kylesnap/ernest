#' Configure an ErnestSampler object
#'
#' Clean and prepare an `ernest_sampler` so that it can be used to generate nested samples.
#'
#' @param object An `ernest_sampler` object.
#' @param refresh If `TRUE`, and if `object` already contains a list of live points,
#' the existing live points will be overwritten.
#' @param ... Ignored.
#'
#' @returns A copy of `object`, invisibly.
#' @export
compile.ernest_sampler <- function(object, refresh = FALSE, ...) {
  object$compile(refresh)
}

#' Internal method for compiling the live sample
#'
#' @returns NULL if live is not NULL and refresh is FALSE, otherwise a new
#' live sample (a list with three elements)
#'
#' @noRd
.compile_sampler <- function(live, lrps, n_points, refresh) {
  if (!is_empty(live) && !refresh) {
    return(NULL)
  }
  live <- list(
    "units" = matrix(NA_real_, nrow = n_points, ncol = lrps$n_dim),
    "points" = matrix(NA_real_, nrow = n_points, ncol = lrps$n_dim),
    "log_lik" = rep(NA_real_, n_points)
  )
  for (i in seq(1:n_points)) {
    live$units[i, ] <- stats::runif(lrps$n_dim)
    live$points[i, ] <- lrps$prior_transform(live$units[i, ])
    live$log_lik[i] <- lrps$log_lik(live$points[i, ])
  }
  live
}

#' Validate the live points in the workspace to ensure no NA, NaN, or Inf values
#' @noRd
validate_wrk <- function(object) {
  if (any(!is.finite(object$wrk$live_points))) {
    cli::cli_abort(
      "Non-finite values found in the live points after `prior_transform` was applied."
    )
  }
  if (any(is.na(object$wrk$live_lik) | is.nan(object$wrk$live_lik))) {
    cli::cli_abort("Generated missing log-likelihood values.")
  }
  if (any(object$wrk$live_lik == Inf)) {
    cli::cli_abort("Generated `Inf` log-likelihood values.")
  }
  if (any(object$wrk$live_lik == -Inf)) {
    cli::cli_warn("Generated `-Inf` log-likelihood values.")
  }
  if (length(unique(object$wrk$live_lik)) == 1) {
    cli::cli_warn("All proposed points have the same log-likelihood value.")
  }

  if ((nrow(object$wrk$live_points) != nrow(object$wrk$live_units)) ||
      (nrow(object$wrk$live_points) != length(object$wrk$live_lik))) {
    cli::cli_abort(
      "The workspace is malconfigured (unequal dimensions between live matrices). Try running `compile(object, refresh = TRUE)`."
    )
  }
  if (length(object$wrk$live_lik) != object$wrk$n_points ||
      object$wrk$n_points != object$n_points) {
    cli::cli_abort(
      "The workspace is malconfigured (unequal number of points). Try running `compile(object, refresh = TRUE)`."
    )
  }
  object
}
