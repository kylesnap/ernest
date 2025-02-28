#' Update the LRPS during a nested sampling run
#'
#' @param x An `ErnestSampler` object
#'
#' @returns Either an updated `ErnestSampler` object, or NULL if sampler
#' cannot be updated.
#' @noRd
update_sampler <- function(x) {
  UseMethod("update_sampler")
}

#' @noRd
#' @export
update_sampler.ErnestSampler <- function(x) {
  return(NULL)
}

#' @noRd
#' @export
update_sampler.RandomWalkCube <- function(x) {
  if (is_empty(x$wrk$args)) {
    x$wrk$args <- c("n_acc" = 0, "n_call" = 0)
    return(x)
  }
  act_acc <- x$wrk$args["n_acc"] / x$wrk$args["n_call"]
  new_epsilon <- x$epsilon * exp((act_acc - x$p_acc) / x$n_dim / x$p_acc)
  x$epsilon <- new_epsilon
  x$wrk$args <- c("n_acc" = 0, "n_call" = 0)
  x
}
