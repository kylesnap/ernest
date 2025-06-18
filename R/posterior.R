#' Internal function for wrapping ernest results in a draws_matrix
#'
#' @param dead dead units from run as matrix.
#' @param live live units from run as matrix.
#' @param live_log_lik log lik. of each live point, as returned from sampler
#' @param log_weight log weight of each point, as returned from sampler
#' @param log_z cumsum of log_weight
#'
#' @noRd
as_draws_matrix_ <- function(dead, live, live_log_lik, varnames) {
  lik_order <- order(live_log_lik)
  points <- rbind(
    dead,
    live[lik_order, ]
  )
  colnames(points) <- varnames
  posterior::as_draws_matrix(points)
}
