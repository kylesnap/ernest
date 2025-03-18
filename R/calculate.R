#' Calculate statistics from an `ernest_sampler` object.
#'
#' @param x An `ernest_sampler` object.
#' @param ... Ignored.
#'
#' @returns A tibble with the following columns:
#' * `log_vol`: The estimated log volume of the prior.
#' * `log_lik`: The likelihood of the live points at that iteration.
#' * `log_evidence`: The evidence estimate.
#' * `log_evidence_err`: The error in the evidence estimate.
#' @export
calculate.ernest_sampler <- function(x, ...) {
  x$calculate()
}
