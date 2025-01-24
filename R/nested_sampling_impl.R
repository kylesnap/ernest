#' Nested sampling internal loop
#'
#' Runs the nested sampling algorithm, using a given sampler and control
#' parameters.
#'
#' @param sampler A likelihood restricted prior sampler.
#' @param control A list of control parameters.
#'
#' @noRd
nested_sampling_impl <- function(sampler, control) {
  log_z <- -1.e300
  log_vol <- 0
  d_log_vol <- log((control$num_points + 1.) / control$num_points)
  worst_lik <- -1.e300

  live_u <- matrix(
    runif(sampler$num_dim * control$num_points),
    nrow = control$num_points,
    ncol = sampler$num_dim
  )
  live_point <- matrix(nrow = control$num_points, ncol = sampler$num_dim)
  live_lik <- rep(NA, control$num_points)
  live_copy <- rep(0, control$num_points)

  for (i in 1:control$num_points) {
    live_point[i, ] <- sampler$prior_transform$fn(live_u[i, ])
    live_lik[i] <- sampler$log_lik(live_point[i, ])
  }

  saved_point <- list()
  saved_wt <- list()
  saved_vol <- list()
  saved_lik <- list()
  saved_call <- list()
  saved_worst <- list()
  saved_copy <- list()
  saved_bound <- list()

  num_call <- 0L
  num_iter <- 1L
  num_update <- 0L
  since_update <- 0L
  unif_proposal <- TRUE
  t0 <- Sys.time()

  if (control$verbose) cli::cli_progress_bar(
    "Nested Sampling",
    type = "custom",
    format = "{cli::pb_spin} Performing Nested Sampling | Iter: {cli::pb_current} | Calls: {num_call} | Remaining ln(z) {prettyunits::pretty_signif(log_z_remain, 3)} [{cli::pb_elapsed}]",
    total = control$max_iter,
    clear = FALSE
  )
  for (num_iter in 1L:control$max_iter) {
    if (control$verbose) cli::cli_progress_update()
    log_z_remain <- max(live_lik) - (num_iter - 1L) / control$num_points
    if (log_add_exp(log_z, log_z_remain) - log_z < control$dlogz) {
      if (control$verbose) cli::cli_progress_done()
      break
    }
    if (num_call > control$max_call) {
      if (control$verbose) cli::cli_progress_done()
      break
    }
    if (num_call >= control$first_update) {
      unif_proposal <- FALSE
    }

    worst <- which.min(live_lik)
    new_worst_lik <- live_lik[worst]

    log_d_vol <- log(sum(exp(c(
      log_vol + d_log_vol, log_vol
    )) * c(0.5, -0.5)))
    log_wt <- log_add_exp(new_worst_lik, worst_lik) + log_d_vol
    log_z <- log_add_exp(log_z, log_wt)
    worst_lik <- new_worst_lik

    saved_point[[num_iter]] <- live_point[worst, ]
    saved_wt[[num_iter]] <- log_wt
    saved_vol[[num_iter]] <- log_vol
    saved_lik[[num_iter]] <- worst_lik

    if (since_update >= control$update_interval) {
      sampler <- update_bounds(sampler, live_point)
      num_update <- num_update + 1L
      since_update <- 1L
    }

    copy <- sample.int(control$num_points, size = 1)
    while (copy == worst && control$num_points != 1) {
      copy <- sample.int(control$num_points, size = 1)
    }

    new <- if (unif_proposal) {
      propose_uniform(sampler, live_lik[worst])
    } else {
      propose_live(sampler, live_u[copy, ], live_lik[worst])
    }

    live_u[worst, ] <- new$unit
    live_point[worst, ] <- new$parameter
    live_lik[worst] <- new$log_lik
    num_call <- num_call + new$num_call
    live_copy[worst] <- copy

    saved_call[[num_iter]] <- num_call
    saved_worst[[num_iter]] <- worst
    saved_copy[[num_iter]] <- live_copy[worst]
    saved_bound[[num_iter]] <- num_update
    log_vol <- log_vol - d_log_vol
  }
  cli::cli_progress_done()
  t1 <- Sys.time()

  list(
    "live_point" = live_point,
    "live_lik" = live_lik,
    "saved_point" = saved_point,
    "saved_wt" = saved_wt,
    "saved_vol" = saved_vol,
    "saved_lik" = saved_lik,
    "saved_calls" = saved_call,
    "saved_worst" = saved_worst,
    "saved_copy" = saved_copy,
    "saved_bound" = saved_bound,
    "log_z" = log_z,
    "log_vol" = log_vol,
    "num_iter" = num_iter,
    "time_elapsed" = difftime(t1, t0, units = "secs")
  )
}

make_cli_str <- function(max_iter, max_calls, dlogz) {
  iter_str <- if (max_iter == .Machine$integer.max) {
    "Iter: {cli::pb_current}"
  } else {
    "Iter: {cli::pb_current}"
  }
  dlogz_str <- if (dlogz == 0) {
    "Remaining ln(z) {prettyunits::pretty_signif(log_z_remain, 3)}"
  } else {
    paste0(
      "Remaining ln(z) {prettyunits::pretty_signif(log_z_remain, 3)} > ",
      prettyunits::pretty_signif(dlogz, 3)
    )
  }
  paste(
    "{cli::pb_spin} Performing Nested Sampling",
    iter_str,
    dlogz_str,
    sep = " | "
  )
}

log_add_exp <- function(log_a, log_b) {
  max(log_a, log_b) + log1p(exp(-abs(log_a - log_b)))
}
