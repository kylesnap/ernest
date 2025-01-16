#' Nested sampling internal loop
#'
#' Runs the nested sampling algorithm, using a given sampler and control
#' parameters.
#'
#' @param sampler An object with the super-class `sampler` that defines the
#' likelihood and prior.
#' @param live_size The number of live points to use in the algorithm.
#' @param dlogz,max_iter,max_call Stopping criteria for the algorithm; Inf
#' should be provided as default
#' @param first_update,update_iter Control parameters for the update of the
#' algorithm's sampler
#' @param verbose Whether to print progress to the console
#'
#' @noRd
nested_sampling_impl <- function(sampler, live_size, dlogz, max_iter, max_call,
                                 first_update, update_iter, verbose) {
  log_z <- -1.e300  # ln(evidence)
  log_vol <- 0 # ln(volume)
  d_log_vol <- log((live_size + 1.) / live_size) # Shrinkage
  worst_lik <- -1.e300 # Worst likelihood

  live_u <- matrix(nrow = live_size, ncol = sampler$num_dim)
  live_point <- matrix(nrow = live_size, ncol = sampler$num_dim)
  live_lik <- rep(NA, live_size)
  num_call <- 0L

  for (i in 1:live_size) {
    particle <- propose(sampler)
    live_u[i, ] <- particle$u_point
    live_point[i, ] <- particle$point
    live_lik[i] <- particle$log_lik
    num_call <- num_call + particle$num_call
  }

  saved_point <- list()
  saved_wt <- list()
  saved_vol <- list()
  saved_lik <- list()
  saved_call <- list()

  num_iter <- 1L
  since_update <- 0L
  for (num_iter in 1L:max_iter) {
    worst <- which.min(live_lik)
    new_worst_lik <- live_lik[worst]

    log_vol <- log_vol - d_log_vol
    log_d_vol <- log(sum(exp(c(log_vol + d_log_vol, log_vol)) * c(0.5, -0.5)))
    log_wt <- log_add_exp(new_worst_lik, worst_lik) + log_d_vol
    log_z <- log_add_exp(log_z, log_wt)
    worst_lik <- new_worst_lik

    saved_point[[num_iter]] <- live_point[worst, ]
    saved_wt[[num_iter]] <- log_wt
    saved_vol[[num_iter]] <- log_vol
    saved_lik[[num_iter]] <- worst_lik
    saved_call[[num_iter]] <- num_call

    if (since_update >= update_iter) {
      sampler <- update_sampler(sampler, live_unit)
      since_update <- 0L
    }

    copy <- sample.int(live_size, size = 1)
    while (copy == worst) {
      copy <- sample.int(live_size, size = 1)
    }
    new <- lrps(sampler, live_u[copy, ], live_lik[worst])

    live_u[worst, ] <- new$u_point
    live_point[worst, ] <- new$point
    live_lik[worst] <- new$log_lik

    num_call <- num_call + new$num_call

    log_vol <- log_vol - (1.0 / live_size)

    # STOP: Estimated fractional remaining evidence below some threshold
    log_z_remain <- max(live_lik) - (num_iter - 1L) / live_size
    if (log_add_exp(log_z, log_z_remain) - log_z < dlogz) {
      break
    }

    # STOP: Maximum number of calls
    if (num_call > max_call) {
      break
    }

    if (num_iter > first_update) {
      since_update <- since_update + 1
    }
  }

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
    "num_iter" = num_iter
  )
}

log_add_exp <- function(log_a, log_b) {
  max(log_a, log_b) + log1p(exp(-abs(log_a - log_b)))
}
