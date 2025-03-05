#' Generate nested samples from an `ErnestSampler` object.
#'
#' @param x An `ErnestSampler` object.
#' @param max_it The maximum number of iterations needed to run the sampler. Must
#' be a number larger than zero.
#' @param max_call Maximum number of calls to the likelihood function. Must
#' be a number larger than zero.
#' @param dlogz The threshold for the remaining prior volume to the total
#' evidence. Must represent a number larger or equal to zero.
#' @param refresh If `TRUE`, and if `object` already contains nested sampling
#' results, then the workspace attached to `wrk` will be overwritten. Else, the
#' run will be continued from the most recent iteration provided by ernest.
#' @param ... Ignored.
#'
#' @return An `ErnestSampler` object containing the results of the nested
#' @export
generate.ErnestSampler <-
  function(x, max_it = Inf, max_call = Inf, dlogz = 0.05, refresh = FALSE,...) {
    # Initialize the run
    if (max_it == Inf) {
      max_it <- .Machine$integer.max
    }
    if (max_call == Inf) {
      max_call <- .Machine$integer.max
    }
    check_number_whole(max_it, min = 1, allow_infinite = FALSE)
    check_number_whole(max_call, min = 1, allow_infinite = FALSE)
    check_number_decimal(dlogz, min = 0, allow_infinite = FALSE)

    # Ensure the environment is properly built
    x <- compile(x, refresh = FALSE)

    time1 <- Sys.time()
    nested_sampling_impl(x, max_it, max_call, dlogz)
  }
