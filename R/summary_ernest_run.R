#' Summarising Nested Sampling Runs
#'
#' @param object An object of class `ernest_run`
#' @param exponentiate Whether or not to exponentiate the fdsafnaslk
#' @param normalise Whether or not to normalise the likelihood values
#' @param ... Ignored.
#' @param x A `summary.ernest_run` object
#' @param digits The number of digits to output
#'
#' @returns A list with the following components
#' @importFrom utils tail
#' @export
summary.ernest_run <- function(object, exponentiate = TRUE, normalise = FALSE, ...) {
  run <- summarise_run_metrics(object, exponentiate, normalise)
  overalls <- if (exponentiate) {
    list(
      "evidence" = tail(run$evidence, 1),
    )
  } else {
    list(
      "log_z" = tail(run$log_evidence, 1),
      "log_z_err" = sqrt(tail(run$log_evidence.var, 1))
    )
  }
  cum_calls <- cumsum(object$progress$calls)
  cum_accept <- cumsum(object$progress$accept)
  cum_acceptance_rate <- cum_accept / cum_calls
  out <- list(
    "information" = tail(object$integration$information, 1),
    "iter" = object$progress$iter,
    "calls" = cumsum(object$progress$calls),
    "n_acceptance" = cumsum(object$progress$accept),
    "prop_acceptance" = cum_acceptance_rate,
    "time" = object$time,
    "run" = run,
    "posterior" = posterior::summarise_draws(
      as_draws_matrix(object),
      posterior::default_summary_measures()
    ),
    "exponentiated" = exponentiate,
    "normalised" = normalise
  )
  structure(c(overalls, out), class = "summary.ernest_run")
}

#' @rdname summary.ernest_run
#' @export
print.summary.ernest_run <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  fmt <- \(x) prettyunits::pretty_signif(x, digits = digits)
  cli::cli_h2("Nested Sampling Run Performed by Ernest")

  cli::cli_h3("Integration Results:")
  cli::cli_dl()
  if (x$exponentiated) {
    cli::cli_li(c(Evidence = "{fmt(x$evidence_)}"))
  } else {
    cli::cli_li(c("Log Evidence" = "{fmt(x$log_z)} \U00B1 {fmt(x$log_z_err)}"))
  }
  cli::cli_li(c(Information = "{fmt(x$information)}"))

  cli::cli_h3("Posterior Distribution")
  cli::cat_print(x$posterior)

  iters <- tail(x$iter, 1)
  calls <- tail(x$calls, 1)

  cli::cli_h3("Run Metrics")
  cli::cli_dl(c(
    Iterations = "{iters}",
    Calls = "{calls}",
    "Efficiency" = "{fmt((iters/calls) * 100)}%",
    "Acceptance Rate" = "{fmt(tail(x$prop_acceptance, 1) * 100)}%",
    Time = "{prettyunits::pretty_dt(as.difftime(x$time))}"
  ))
  invisible(x)
}
