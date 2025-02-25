#' Update the LRPS during a nested sampling run
#'
#' @param x An `ErnestSampler` object
#'
#' @returns Either an updated `ErnestSampler` object, or NULL if sampler
#' cannot be updated.
#' @noRd
#' @export
update_sampler <- function(x) {
  UseMethod("update_sampler")
}

#' @noRd
#' @export
update_sampler.ErnestSampler <- function(x) {
  return(NULL)
}
