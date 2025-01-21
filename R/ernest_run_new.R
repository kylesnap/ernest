#' Construct a ernest run object
#'
#' @return A validated ernest_run object
new_ernest_run <- function(sampler, control, result) {
  num_points <- control$num_points
  log_vols <- log(1. - 1:num_points / (num_points + 1.)) + result$log_vol

  ord_lik <- order(result$live_lik)
  sort_lik <- sort(result$live_lik)
  ord_points <- result$live_point[ord_lik, ]
  max_lik <- max(result$live_lik)

  num_iter <- result$num_iter
  for (i in 1:num_points) {
    cur_lik <- sort_lik[i]
    log_wt <- log_vols[i] + cur_lik

    result$saved_point[[i + num_iter]] <- ord_points[i, ]
    result$saved_wt[[i + num_iter]] <- log_wt
    result$saved_vol[[i + num_iter]] <- log_vols[i]
    result$saved_lik[[i + num_iter]] <- cur_lik
  }

  log_lik <- list_c(result$saved_lik)
  log_vol <- list_c(result$saved_vol)

  integration <- compute_integral(log_lik, log_vol)
  sample <- do.call(rbind, result$saved_point)
  colnames(sample) <- sampler$prior_transform$names
  sample <- tibble::tibble(
    tibble::as_tibble(sample),
    log_lik,
    log_vol,
    "weights" = exp(integration$log_wt - max(integration$log_z))
  )
  progress <- tibble::tibble(
    "num_calls" = list_c(result$saved_calls)
  )

  structure(
    list(
      "sampler" = sampler,
      "sample" = sample,
      "integration" = integration,
      "progress" = progress,
      "time" = result$time
    ),
    class = "ernest_run"
  )
}

