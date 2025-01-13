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
#' @return A list with the following components:
#'  - dead: A list of dead points, each with the following components:
#'  unit_point, point, log_lik, log_vol, log_wt, log_z, log_z_var, h,
#'  parent_iter, parent_id, num_call, sampler_updates
#'  - live_u: The unit-cube expression of the live points
#'  - live_point: The original parameter values of the live points
#'  - live_lik: The log-likelihoods of the live points
#'  - num_call: The total number of calls to the likelihood function
#'  - num_iter: The total number of iterations
#'
#' @noRd
nested_sampling_impl <- function(sampler, live_size, dlogz, max_iter, max_call,
                                 first_update, update_iter, verbose) {
  dead <- list()
  h <- 0.  # Information
  log_z <- -1.e300  # ln(evidence)
  log_z_var <- 0  # Var[ln(evidence)]
  log_vol <- 0 # ln(volume)
  log_lik_star <- -1.e300  # ln(likelihood^*), i.e. the criterion
  delta_logz <- 1.e300  # ln(ratio) of total/current evidence
  dlv <- log1p(live_size) - log(live_size)  # Shrinkage per iteration

  live_u <- matrix(nrow = live_size, ncol = sampler$num_dim)
  live_point <- matrix(nrow = live_size, ncol = sampler$num_dim)
  live_lik <- rep(NA, live_size)
  live_birth <- rep(0, live_size)
  live_parent <- rep(0, live_size)

  for (i in 1:live_size) {
    particle <- propose(sampler)
    live_u[i, ] <- particle$u_point
    live_point[i, ] <- particle$point
    live_lik[i] <- particle$log_lik
  }

  # The Nested Sampling Algorithm
  num_call <- 1
  num_iter <- 1
  num_update <- 0
  print(num_iter < max_iter && num_call < max_call)
  while (num_iter < max_iter && num_call < max_call) {
    delta_logz <- log_add_exp(0, max(live_lik) + log_vol - log_z)
    if (delta_logz < dlogz) {
      rlang::inform("Terminating: delta_logz < 0.01")
      break
    }
    worst <- which.min(live_lik)
    log_lik_new <- live_lik[worst]
    log_vol <- log_vol - dlv

    # Update the integral
    logdvol <- log(sum(c(0.5, -0.5) * exp(c(log_vol + dlv, log_vol))))
    log_wt <- log_add_exp(log_lik_new, log_lik_star) + logdvol
    log_z_new <- log_add_exp(log_z, log_wt)
    lzterm <- exp(log_lik_star - log_z_new + logdvol) * log_lik_star +
      exp(log_lik_new - log_z_new + logdvol) * log_lik_new
    h_new <- lzterm + exp(log_z - log_z_new) * (h + log_z) - log_z_new
    dh <- h_new - h

    log_lik_star <- log_lik_new
    log_z <- log_z_new
    log_z_var <- log_z_var + dh * dlv
    h <- h_new

    dead[[i]] <- list(
      "unit_point" = live_u[worst, ],
      "point" = live_point[worst, ],
      "log_lik" = log_lik_star,
      "log_vol" = log_vol,
      "log_wt" = log_wt,
      "log_z" = log_z,
      "log_z_var" = log_z_var,
      "h" = h,
      "parent_iter" = live_birth[worst],
      "parent_id" = live_parent[worst],
      "num_call" = num_call,
      "sampler_updates" = num_update
    )

    copy <- 1 + floor(live_size * runif(1, 0.0, 1.0)) %% live_size
    while (copy == worst && live_size > 1) {
      copy <- 1 + floor(live_size * runif(1, 0.0, 1.0)) %% live_size
    }

    new <- lrps(sampler, live_u[copy, ], log_lik_new)
    live_u[worst, ] <- new$u_point
    live_point[worst, ] <- new$point
    live_lik[worst] <- new$log_lik
    live_birth[worst] <- i
    live_parent[worst] <- copy

    num_call <- num_call + new$num_call
    num_iter <- num_iter + 1
  }

  list(
    "dead" = dead,
    "live_u" = live_u,
    "live_point" = live_point,
    "live_lik" = live_lik,
    "num_call" = num_call,
    "num_iter" = num_iter
  )
}

log_add_exp <- function(log_a, log_b) {
  max(log_a, log_b) + log1p(exp(-abs(log_a - log_b)))
}
