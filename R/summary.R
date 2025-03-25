#' Summarise an `ernest_sampler` object
#'
#' [summary()] method for `ernest_sampler` objects, producing an `S3` object
#' describing a unit's run.
#'
#' @param object An `ernest_sampler` object.
#' @param ... Must be empty.
#'
#' @returns A list with class `summary.ernest_sampler` containing these named elements:
#' * n_points: The number of live points used in the run; a scalar integer.
#' * n_iter: The number of iterations; a scalar integer.
#' * n_call: The total number of function calls; a scalar integer.
#' * eff: The overall sampling efficiency; a scalar double.
#' * log_weight: The posterior log-weight associated with each point; a vector of doubles.
#' * log_lik: The log-likelihood associated with each point; a vector of doubles.
#' * log_vol: The estimated log-volume associated with each point; a vector of doubles.
#' * log_z: The cumulative evidence estimate at each iteration; a vector of doubles.
#' * log_z_err: The estimated error (standard deviation) of `log_z`; a vector of doubles.
#' * information: The estimated information at each iteration; a vector of doubles.
#' @export
summary.ernest_sampler <- function(object,
                                   ...) {
  check_dots_empty()
  object$summary()
}

#' Format a summary.ernest_sampler
#' @export
#' @noRd
format.summary.ernest_sampler <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cli::cli_format_method({
    cli::cli_h1("Nested Sampling Run from {.pkg ernest}")
    cli::cli_h2("Sampler Info:")
    cli::cli_dl(c(
      "No. Live Points" = "{.val {x$n_points}}",
      "No. Iterations" = "{.val {x$n_iterations}}",
      "No. Calls" = "{.val {x$n_calls}}"
    ))
    if (is_empty(x$eff)) {
      cli::cli_alert_info("No samples generated yet.")
    } else {
      cli::cli_h2("Results")
      min_l <- prettyunits::pretty_round(min(x$log_lik), digits = digits)
      max_l <- prettyunits::pretty_round(max(x$log_lik), digits = digits)
      log_z <- prettyunits::pretty_round(tail(x$log_z, 1), digits = digits)
      log_z_err <- prettyunits::pretty_round(tail(x$log_z_err, 1), digits = digits)
      cli::cli_dl(c(
        "Efficiency" = "{.val {x$eff}}",
        "Log. Likelihood" = "[{min_l}, {max_l}]",
        "Log. Evidence (\U00B1 SD)" = "{log_z} (\U00B1 {log_z_err})"
      ))
    }
  })
}

#' Print a summary.ernest_sampler
#' @export
#' @noRd
print.summary.ernest_sampler <- function(x, ...) {
  cat(format(x), sep = "\n")
}
