#' @importFrom generics calculate
#' @export
generics::calculate

#' Calculate the evidence integral.
#'
#' @param add_live Whether to include the live points in the calculation.
#' @include ernest_run-new.R
#' @export
S7::method(calculate, ErnestRun) = function(x, add_live = TRUE) {
  if (x@env$tot_it == 0) {
    cli::cli_warn("There are no samples within this run yet. Have you run `run()?`")
    return(NULL)
  }

  dead_lik <- vctrs::field(x@env$history, "log_lik")
  dead_vol <- vctrs::field(x@env$history, "log_vol")

  live_lik <- if (add_live) {
    sort(x@env$live_lik)
  } else {
    NULL
  }

  live_vol <- if (add_live) {
    log1p((-1 - x@n_points)^(-1) * seq_len(x@n_points)) +
      x@env$log_vol
  } else {
    NULL
  }

  log_lik <- c(dead_lik, live_lik)
  log_vol <- c(dead_vol, live_vol)
  compute_integral(log_lik, log_vol)
}
