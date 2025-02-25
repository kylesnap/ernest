#' Update the LRPS during a nested sampling run
#'
#' @param x An `ErnestSampler` object
#'
#' @returns Either an updated `ErnestSampler` object, or NULL if sampler
#' cannot be updated.
#' @noRd
update_sampler <- S7::new_generic("update_sampler", "x")

S7::method(update_sampler, ErnestSampler) <- function(x, live) {
  return(NULL)
}
