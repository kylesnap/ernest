#' Print a summary of the results
#'
#' @param run An `ernest` run
#' @param digits The number of digits to output
#'
#' @returns The run object, invisibly.
#' @export
print.ernest_run <- function(run, digits = max(3, getOption("digits") - 3)) {
  cli::cli_h3("Nested Sampling Run with `ernest`")
  num_points <- run$control$num_points
  num_iter <- nrow(run$progress)
  num_calls <- tail(run$progress$num_calls, 1)
  log_z <- tail(run$integration$log_z, 1)
  log_z_var <- tail(run$integration$log_z_var, 1)
  cli::cli_dl(c(
    "Live Points" = "{prettyunits::pretty_num(num_points)}",
    "Iterations" = "{prettyunits::pretty_num(num_iter)}",
    "Calls" = "{prettyunits::pretty_num(num_calls)}",
    "Efficiency" = "{prettyunits::pretty_round(num_iter/num_calls * 100, digits = digits)}%",
    "Log. Evidence" = "{prettyunits::pretty_signif(log_z, digits = digits)} \U00B1
    {prettyunits::pretty_signif(log_z_var, digits = digits)}"
  ))
}

#' Return the number of posterior particles within an ernest sampling run
#'
#' @param run An `ernest_run` object
#'
#' @return A single integer, describing the number of rows in the posterior sample
#' @export
nobs.ernest_run <- function(run) {
  nrow(run$sample)
}
