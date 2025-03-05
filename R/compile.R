#' Configure an ErnestSampler object
#'
#' Ernest conducts nested sampling in an enviroment bound to the nested sampler.
#' This function configures this environment so that it contains the necessary
#' variables.
#'
#' When [generate()] has been called on the `ErnestSampler` object before, it
#' will c
#'
#' @param object An ErnestSampler or ErnestSampler object.
#' @param refresh If `TRUE`, and if `object` already contains an ErnestWorkspace,
#' then the workspace attached to `wrk` will be overwritten. If `FALSE`, then
#' the workspace will be cloned and bound to compile's output.
#' @param ... Ignored.
#'
#' @return A copy of `object`, with an `ErnestWorkspace` bound to the `wrk`
#' field.
#' @export
compile.ErnestSampler <- function(object, refresh = FALSE, ...) {
  if (is_empty(object$wrk)) {
    object$wrk <- ErnestWorkspace$new(
      object$log_lik,
      object$prior_transform,
      object$n_dim,
      object$n_points
    )
    return(validate_wrk(object))
  }

  new <- object
  new$wrk <- if (refresh) {
    if (new$verbose) {
      cli::cli_inform("Run will start from 0 iterations.")
    }
    ErnestWorkspace$new(
      object$log_lik,
      object$prior_transform,
      object$n_dim,
      object$n_points
    )
  } else {
    if (new$verbose) {
      cli::cli_inform("Run will start from {new$wrk$n_iter} iterations.")
    }
    new$wrk$clone(deep = TRUE)
  }
  validate_wrk(new)
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
