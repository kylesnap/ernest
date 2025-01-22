#' Construct a ernest run object
#'
#' @return A validated ernest_run object
new_ernest_run <- function(sampler, control, result) {
  sample <- add_live_points(result, sampler$prior_transform$names)
  integration <- compute_integral(sample$log_lik, sample$log_vol)
  sample$weight <- exp(integration$log_wt - tail(integration$log_z, 1))
  progress <- tibble::tibble(
    "num_calls" = list_c(result$saved_calls),
    ".id" = list_c(result$saved_worst),
    "parent_id" = list_c(result$saved_copy),
    "bound_iter" = list_c(result$saved_bound)
  )
  structure(
    list(
      "sampler" = sampler,
      "sample" = sample,
      "integration" = integration,
      "progress" = progress,
      "time" = result$time,
      "control" = control
    ),
    class = "ernest_run"
  )
}

add_live_points <- function(result, names) {
  n <- nrow(result$live_point)

  live_log_vols <- log(1 - seq_len(n) / (n + 1)) + result$log_vol
  log_vol <- c(list_c(result$saved_vol), live_log_vols)

  log_lik <- c(list_c(result$saved_lik), sort(result$live_lik))
  ids <- c(list_c(result$saved_worst), seq_len(n)[order(result$live_lik)])

  points <- rbind(
    do.call(rbind, result$saved_point),
    result$live_point[order(result$live_lik), ]
  )
  colnames(points) <- names
  tibble::tibble(
    ".id" = ids,
    tibble::as_tibble(points),
    log_lik,
    log_vol
  )
}

