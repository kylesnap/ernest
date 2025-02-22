#' Generate nested samples from an `ErnestLRPS` object.
#'
#' @param x An `ErnestLRPS` object.
#' @param maxit The maximum number of iterations needed to run the sampler. Must
#' be a number larger than zero.
#' @param maxcall Maximum number of calls to the likelihood function. Must
#' be a number larger than zero.
#' @param dlogz The threshold for the remaining prior volume to the total
#' evidence. Must represent a number larger or equal to zero.
#'
#' @return An `ErnestRun` object containing the results of the nested
#' sampling run.
NULL

#' @rdname generate
#' @export
S7::method(generate, ErnestLRPS) <-
  function(x, max_it = Inf, max_call = Inf, dlogz = 0.05) {
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

  # --- Cleanup activities here --- #
  x <- compile(x)

  time1 <- Sys.time()
  nested_sampling_impl(x, max_it, max_call, dlogz)
  #sampler@wrk$time <- as.difftime(Sys.time() - time1, units = "secs")
}
