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
update_sampler.ernest_lrps <- function(x) {
  NULL
}

#' @noRd
#' @export
update_sampler.rw_cube <- function(x) {
  act_acc <- x$args$n_acc / x$args$n_call
  new_epsilon <- tail(x$epsilon, 1)[[1]] * exp((act_acc - x$p_acc) / x$n_dim / x$acceptance_rate)
  x$epsilon <- append(x$epsilon, new_epsilon)
  env_poke(x$args, "n_acc", 0)
  env_poke(x$args, "n_call", 0)
  x
}
