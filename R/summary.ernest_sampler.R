#' @export
summary.ernest_sampler <- function(object, ...) {
  check_dots_empty()
  object$summary()
}

#' @noRd
new_es_summary <- function(self, private) {
  calc <- list()
  samples_orig <- matrix()
  samples_unit <- matrix()
  if (self$niterations == 0) {
    calc <- NULL
    suppressMessages({
      samples_orig <- self$get_live_points("original", reorder = TRUE)
      samples_unit <- self$get_live_points("unit", reorder = TRUE)
    })
  } else {
    calc <- self$calculate()
    samples_orig <- rbind(
      self$get_dead_points("original"),
      self$get_live_points("original", reorder = TRUE)
    )
    samples_unit <- rbind(
      self$get_dead_points("unit"),
      self$get_live_points("unit", reorder = TRUE)
    )
  }
  result <- if (is_null(calc)) {
    list("log_likelihood" = sort(private$live$log_lik))
  } else {
    vctrs::list_of(
      "log_likelihood" = calc$log_likelihood,
      "log_volume" = calc$log_volume,
      "log_importance_weight" = calc$log_weight - max(calc$log_evidence),
      "log_evidence" = calc$log_evidence,
      "log_evidence.sd" = sqrt(calc$log_evidence.var),
      "information" = calc$information,
      .ptype = double()
    )
  }
  sampling_info <- list(
    "n_points" = private$n_points,
    "n_iterations" = self$niterations,
    "n_calls" = self$ncalls,
    sampler = list(
      "lrps" = private$lrps$clone(deep = TRUE),
      "first_update" = private$n_points,
      "update_interval" = private$update_interval
    )
  )
  structure(
    c(
      sampling_info,
      "samples" = list(
        "original" = samples_orig,
        "unit" = samples_unit
      ),
      result
    ),
    class = c("summary.ernest_sampler")
  )
}

#' Format a summary.ernest_sampler
#' @export
#' @noRd
format.summary.ernest_sampler <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_h1("Nested Sampling Run from {.pkg ernest}")
    cli::cli_dl(c(
      "No. Points" = x$n_points,
      "No. Iterations" = x$n_iterations,
      "No. Calls" = x$n_calls
    ))
    cli::cli_h3("Sampler")
    cli::cat_print(x$sampler$lrps)
    if (x$n_iterations == 0) {
      cli::cli_alert_info("No iterations have been performed yet.")
    } else {
      cli::cli_h3("Results")
      log_range <- prettyNum(range(x$log_likelihood))
      cli::cli_dl(c(
        "Ln. Likelihood" = "[{log_range[1]}, {log_range[2]}]",
        "Ln. Volume" = "{prettyNum(tail(x$log_volume, 1))}",
        "Ln. Evidence" = "{prettyNum(tail(x$log_evidence, 1))} \U00B1 {prettyNum(tail(x$log_evidence.sd, 1))}"
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
