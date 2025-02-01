#' Construct a ernest run object
#' @noRd
new_ernest_run <- function(sampler, control, result) {
  live_indx <- order(result$live_lik)
  log_liks <- c(list_c(result$saved_lik), result$live_lik[live_indx])
  live_vols <- log1p((-1 - control$num_points)^(-1) * seq_len(control$num_points)) + result$log_vol
  log_vols <- c(list_c(result$saved_vol), live_vols)

  integration <- compute_integral(log_liks, log_vols)
  points <- rbind(
    do.call(rbind, result$saved_point),
    result$live_point[order(result$live_lik), ]
  )
  colnames(points) <- sampler$prior_transform$names
  samples <- tibble::tibble(
    ".ids" = c(list_c(result$saved_worst), live_indx),
    tibble::as_tibble(points),
    ".log_weight" = integration$log_weight - tail(integration$log_z, 1)
  )
  progress <- tibble::tibble(
    "num_calls" = list_c(result$saved_calls),
    ".id" = list_c(result$saved_worst),
    "parent_id" = list_c(result$saved_copy),
    "bound_iter" = list_c(result$saved_bound)
  )
  structure(
    list(
      "sampler" = sampler,
      "samples" = samples,
      "integration" = integration,
      "progress" = progress,
      "time" = result$time,
      "control" = control
    ),
    class = "ernest_run"
  )
}

#' Print a summary of the results
#'
#' @param x An `ernest` run
#' @param digits The number of digits to output
#' @param ... Ignored
#'
#' @returns The run object, invisibly.
#' @importFrom utils tail
#' @export
print.ernest_run <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cli::cli_h3("Nested Sampling Run with `ernest`")
  num_points <- x$control$num_points
  num_iter <- nrow(x$progress)
  num_calls <- tail(x$progress$num_calls, 1)
  log_z <- tail(x$integration$log_z, 1)
  log_z_var <- tail(x$integration$log_z_var, 1)
  cli::cli_dl(c(
    "Live Points" = "{prettyunits::pretty_num(num_points)}",
    "Iterations" = "{prettyunits::pretty_num(num_iter)}",
    "Calls" = "{prettyunits::pretty_num(num_calls)}",
    "Efficiency" = "{prettyunits::pretty_round(num_iter/num_calls * 100, digits = digits)}%",
    "Log. Evidence" = "{prettyunits::pretty_signif(log_z, digits = digits)} \U00B1
    {prettyunits::pretty_signif(log_z_var, digits = digits)}"
  ))
}

