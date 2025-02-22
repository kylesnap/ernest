#' Update the LRPS during a nested sampling run
#'
#' @param x An `ErnestLRPS` object
#'
#' @returns Either an updated `ErnestLRPS` object, or NULL if sampler
#' cannot be updated.
update_sampler <- S7::new_generic("update_sampler", "x")

S7::method(update_sampler, ErnestLRPS) <- function(x, live) {
  return(NULL)
}
