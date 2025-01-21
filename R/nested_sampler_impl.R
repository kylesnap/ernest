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
  num_call <- 0L

  for (i in 1:control$num_points) {
    live_point[i, ] <- sampler$prior_transform$fn(live_u[i, ])
    live_lik[i] <- sampler$log_lik(live_point[i, ])
  }

  saved_point <- list()
  saved_wt <- list()
  saved_vol <- list()
  saved_lik <- list()
  saved_call <- list()

  num_iter <- 1L
  since_update <- -num_call
  unif_proposal <- FALSE
  t0 <- Sys.time()
  for (num_iter in 1L:control$max_iter) {
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
    saved_call[[num_iter]] <- num_call

    if (since_update >= control$update_interval || unif_proposal) {
      sampler <- update_bounds(sampler, live_point)
      since_update <- 1L
    }

    # NOTE: This will break if num_points is equal to one...
    copy <- sample.int(control$num_points, size = 1)
    while (copy == worst) {
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
    log_vol <- log_vol - d_log_vol

    log_z_remain <- max(live_lik) - (num_iter - 1L) / control$num_points
    if (log_add_exp(log_z, log_z_remain) - log_z < control$dlogz) {
      break
    }

    num_call <- num_call + new$num_call
    since_update <- since_update + num_call
    if (!unif_proposal && since_update > 0) {
      unif_proposal <- TRUE
    }
    if (num_call > control$max_call) {
      break
    }
  }
  t1 <- Sys.time()

  list(
    "live_point" = live_point,
    "live_lik" = live_lik,
    "saved_point" = saved_point,
    "saved_wt" = saved_wt,
    "saved_vol" = saved_vol,
    "saved_lik" = saved_lik,
    "saved_calls" = saved_call,
    "log_z" = log_z,
    "log_vol" = log_vol,
    "num_iter" = num_iter,
    "time_elapsed" = as.numeric(difftime(t1, t0, units = "secs"))
  )
}

log_add_exp <- function(log_a, log_b) {
  max(log_a, log_b) + log1p(exp(-abs(log_a - log_b)))
}
