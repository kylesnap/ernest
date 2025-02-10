#' Construct a ernest run object
#' @noRd
new_ernest_run <- function(sampler, control, result) {
  live_indx <- order(result$live_lik)
  log_liks <- c(list_c(result$saved_lik), result$live_lik[live_indx])
  live_vols <- log1p((-1 - control$num_points)^(-1) * seq_len(control$num_points)) + result$log_vol
  log_vols <- c(list_c(result$saved_vol), live_vols)

  integration <- compute_integral(log_liks, log_vols)
  points <- list_to_matrix(
    result$saved_point,
    result$live_point[order(result$live_lik), ],
    sampler$num_dim
  )
  colnames(points) <- sampler$prior_transform$names
  samples <- tibble::tibble(
    ".id" = c(list_c(result$saved_index), live_indx),
    tibble::as_tibble(points),
    ".log_weight" = integration$log_weight - tail(integration$log_z, 1)
  )

  iter <- c(1L:length(result$saved_index), rep(length(result$saved_index), control$num_points))
  progress <- tibble::tibble(
    "iter" = as.integer(iter),
    "worst_id" = c(list_c(result$saved_index), live_indx),
    "calls" = c(list_c(result$saved_calls), rep(0L, control$num_points)),
    "num_live" = c(
      rep(as.integer(control$num_points), length(result$saved_index)),
      control$num_points:1L
    ),
    "accept" = c(list_c(result$saved_calls), rep(0L, control$num_points)),
    "replacement_id" = c(list_c(result$saved_parent), rep(0L, control$num_points)),
    "sampler_iter" = c(list_c(result$saved_sampler), rep(0L, control$num_points))
  )
  cands <- do.call(rbind, result$saved_cand)
  colnames(cands) <- paste0("cand_", sampler$prior_transform$names)
  new_points <- do.call(rbind, result$saved_new)
  colnames(new_points) <- paste0("new_", sampler$prior_transform$names)
  evolutions <- tibble::tibble(
    tibble::as_tibble(cands),
    tibble::as_tibble(new_points)
  )

  structure(
    list(
      "samples" = samples,
      "integration" = integration,
      "progress" = progress,
      "ernest_sampler" = sampler,
      "time" = result$time,
      "evolutions" = evolutions,
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
  num_iter <- tail(x$progress$iter, 1)
  num_calls <- sum(x$progress$calls)
  if (is_empty(x$integration$log_z)) {
    cli::cli_warning("No evidence has been computed.")
    return(invisible(x))
  }
  log_z <- tail(x$integration$log_z, 1)
  log_z_err <- sqrt(tail(x$integration$log_z_var, 1))
  cli::cli_dl(c(
    "Live Points" = "{prettyunits::pretty_num(num_points)}",
    "Iterations" = "{prettyunits::pretty_num(num_iter)}",
    "Calls" = "{prettyunits::pretty_num(num_calls)}",
    "Log. Evidence" = "{prettyunits::pretty_signif(log_z, digits = digits)} \U00B1
    {prettyunits::pretty_signif(log_z_err, digits = digits)}"
  ))
}

# Merge a list and a matrix into a single matrix
list_to_matrix <- function(lst_a, mat_b, inner_dim) {
  if (inner_dim == 1) {
    tibble::as_tibble_col(
      c(list_c(lst_a), mat_b),
      column_name = "X"
    )
  } else {
    rbind(
      do.call(rbind, lst_a),
      mat_b
    )
  }
}
