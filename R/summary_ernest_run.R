#' Summarising Nested Sampling Runs
#'
#' @param object An object of class `ernest_run`
#' @param exponentiate Whether or not to exponentiate the fdsafnaslk
#' @param normalise Whether or not to normalise the likelihood values
#'
#' @returns A list with the following components
#' @export
summary.ernest_run <- function(object, exponentiate = TRUE, normalise = FALSE) {
  run <- summarise_run_metrics(object, exponentiate, normalise)
  overalls <- if (exponentiate) {
    list(
      "evidence" = tail(run$evidence, 1),
      "evidence.err" = sqrt(tail(run$evidence.var, 1))
    )
  } else {
    list(
      "log_evidence" = tail(run$log_evidence, 1),
      "log_evidence.err" = sqrt(tail(run$log_evidence.var, 1))
    )
  }
  out <- list(
    "information" = tail(object$integration$information, 1),
    "iter" = nrow(object$progress),
    "calls" = tail(object$progress$num_calls, 1),
    "efficiency" = 100 * 1:nrow(object$progress) / object$progress$num_calls,
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
print.summary.ernest_run <- function(x, digits = max(3, getOption("digits") - 3)) {
  fmt <- \(x) prettyunits::pretty_signif(x, digits = digits)
  cli::cli_h2("Nested Sampling Run Performed by Ernest")

  cli::cli_h3("Integration Results:")
  cli::cli_dl()
  if (x$exponentiated) {
    cli::cli_li(c(Evidence = "{fmt(x$evidence)} \U00B1 {fmt(x$evidence.err)}"))
  } else {
    cli::cli_li(c("Log Evidence" = "{fmt(x$log_evidence)} \U00B1 {fmt(x$log_evidence.err)}"))
  }
  cli::cli_li(c(Information = "{fmt(x$information)}"))

  cli::cli_h3("Posterior Distribution")
  cli::cat_print(x$posterior)

  cli::cli_h3("Run Metrics")
  cli::cli_dl(c(
    Iterations = "{x$iter}",
    Calls = "{x$calls}",
    "Overall Efficiency" = "{prettyunits::pretty_round(tail(x$efficiency, 1), digits)}%",
    Time = "{prettyunits::pretty_dt(as.difftime(x$time))}"
  ))
  invisible(x)
}
