#' @importFrom generics glance
#' @export
generics::glance

#' Print important summary information about the nested sampling run
#'
#' @param add_live Whether to include the live points in the calculation.
#' @include ernest_run-new.R
#' @export
S7::method(glance, ErnestRun) <- function(x, add_live = TRUE) {
  empty <- x@env$tot_it == 0L

  integral <- if (empty) NULL else calculate(x)[nrow(calculate(x)),]
  tibble::tibble_row(
    "n_points" = x@n_points,
    "n_par" = x@sampler@num_dim,
    "iterations" = x@env$tot_it,
    "calls" = x@env$tot_call,
    "time" = if (empty) NULL else x@env$time,
    "n_updates" = if (empty) NULL else x@env$sampler_updates,
    "log_z" = if (empty) NULL else integral$log_z,
    "log_z_err" = if (empty) NULL else sqrt(integral$log_z_var),
    "information" = if (empty) NULL else integral$information
  )
}
