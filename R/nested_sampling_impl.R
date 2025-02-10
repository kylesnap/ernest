#' Perform a nested sampling run
#'
#' @noRd
nested_sampling_impl <- function(sampler, control) {
  live_u <- matrix(
    stats::runif(sampler$num_dim * control$num_points),
    nrow = control$num_points,
    ncol = sampler$num_dim
  )
  live_point <- matrix(nrow = control$num_points, ncol = sampler$num_dim)
  live_lik <- rep(NA, control$num_points)

  saved_point <- list()
  saved_cand <- list()
  saved_new <- list()
  saved_vol <- list()
  saved_lik <- list()
  saved_index <- list()
  saved_calls <- list()
  saved_accept <- list()
  saved_parent <- list()
  saved_sampler <- list()

  for (i in 1:control$num_points) {
    live_point[i, ] <- sampler$prior_transform$fn(live_u[i, ])
    live_lik[i] <- sampler$log_lik(live_point[i, ])
  }

  t0 <- Sys.time()
  # Set starting values
  log_z <- -1.e300
  log_vol <- 0
  d_log_vol <- log((control$num_points + 1.) / control$num_points)
  d_log_z <- 1.e300
  worst_lik <- -1.e300

  iter <- 0L
  calls <- 0L
  sampler_iter <- 0L

  since_update <- 0L
  unif_proposal <- if (control$first_update == 0) FALSE else TRUE

  if (control$verbose) cli::cli_progress_bar(
    "Nested Sampling",
    type = "custom",
    format = "{cli::pb_spin} Performing Nested Sampling | Iter: {cli::pb_current} | Calls: {calls} | Remaining ln(z) {prettyunits::pretty_signif(d_log_z, 3)} [{cli::pb_elapsed}]",
    format_done = "{cli::symbol$tick} Nested Sampling Complete | Iter: {iter} | Calls: {calls} | ln(z) = {log_z}",
    format_failed = "{cli::symbol$cross} Nested Sampling Failed | Iter: {iter} | Calls: {calls}",
    total = control$max_iter
  )

  for (iter in 1L:control$max_iter) {
    # Check stopping conditions
    d_log_z <- logaddexp(0, max(live_lik) + log_vol - log_z)
    if (calls > control$max_call) {
      if (control$verbose) cli::cli_progress_done()
      break
    }
    if (d_log_z < control$dlogz) {
      if (control$verbose) cli::cli_progress_done()
      break
    }
    if (control$verbose) cli::cli_progress_update()

    # Get the worst point in the likelihood
    worst <- which.min(live_lik)
    new_worst_lik <- live_lik[worst]

    # Shrink the prior volume
    log_vol <- log_vol - d_log_vol

    # Update the sampler
    if (unif_proposal && calls > control$first_update) {
      unif_proposal <- FALSE
      sampler_iter <- 1L
      since_update <- 0L
    }
    if (!inherits_any(sampler, c("unif_cube", "rw_cube")) &&
          (since_update == 0 || since_update >= control$update_interval)) {
      sampler <- update_bounds(sampler, live_lik)
      since_update <- 0L
      num_update <- sampler_iter + 1L
    }

    # Get a new particle
    copy <- sample.int(control$num_points, size = 1)
    while (copy == worst && control$num_points != 1) {
      copy <- sample.int(control$num_points, size = 1)
    }

    new <- if (unif_proposal) {
      propose_uniform(sampler, live_lik[worst])
    } else {
      propose_live(sampler, live_u[copy, ], new_worst_lik)
    }
    calls <- calls + new$num_calls

    # Update the integral
    # Numerically stable log([exp(logvol + dlv) - exp(logvol)]/2)
    # TODO: Check this for accuracy
    log_d_vol <- log(0.5 * expm1(d_log_vol)) + log_vol
    log_wt <- logaddexp(new_worst_lik, worst_lik) + log_d_vol
    log_z <- logaddexp(log_z, log_wt)
    worst_lik <- new_worst_lik

    saved_point[[iter]] <- live_point[worst, ]
    saved_cand[[iter]] <- live_point[copy, ]

    saved_vol[[iter]] <- log_vol
    saved_lik[[iter]] <- worst_lik
    saved_index[[iter]] <- worst
    saved_calls[[iter]] <- new$num_calls
    saved_accept[[iter]] <- new$accept
    saved_parent[[iter]] <- copy
    saved_sampler[[iter]] <- sampler_iter

    # Copy new over bad object
    live_u[worst, ] <- new$unit
    live_point[worst, ] <- new$parameter
    live_lik[worst] <- new$log_lik

    saved_new[[iter]] <- live_point[worst, ]
  }

  t1 <- Sys.time()

  list(
    "live_point" = live_point,
    "live_lik" = live_lik,
    "saved_point" = saved_point,
    "saved_cand" = saved_cand,
    "saved_new" = saved_new,
    "saved_vol" = saved_vol,
    "saved_lik" = saved_lik,
    "saved_index" = saved_index,
    "saved_calls" = saved_calls,
    "saved_accept" = saved_accept,
    "saved_parent" = saved_parent,
    "saved_sampler" = saved_sampler,
    "log_z" = log_z,
    "log_vol" = log_vol,
    "iter" = iter,
    "time_elapsed" = difftime(t1, t0, units = "secs")
  )
}
