#' Generate nested samples from an `Ernest` object.
#'
#' @param x An `ErnestSampler` object.
#' @param maxit The maximum number of iterations needed to run the sampler. Must
#' be a number larger than zero.
#' @param maxcall Maximum number of calls to the likelihood function. Must
#' be a number larger than zero.
#' @param dlogz The threshold for the remaining prior volume to the total
#' evidence. Must represent a number larger or equal to zero.
#'
#' @return An `ErnestRun` object containing the results of the nested
#' sampling run.
#'
#' @include sampler.R
#' @rdname generate
#' @importFrom generics generate
#' @export
generics::generate

#' @name generate
#' @export
S7::method(generate, ErnestLRPS) <-
  function(x, max_it = Inf, max_call = Inf, dlogz = 0.05) {
  # Initialize the run
  if (max_it == Inf) {
    max_it <- .Machine$integer.max
  }
  if (max_call == Inf) {
    max_call <- .Machine$integer.max
  }
  check_number_whole(max_it, min = 1, allow_infinite = FALSE)
  check_number_whole(max_call, min = 1, allow_infinite = FALSE)
  check_number_decimal(dlogz, min = 0, allow_infinite = FALSE)

  # --- Cleanup activities here --- #
  x <- compile(x)

  env_bind(
    x@wrk,
    log_vol = 0,
    log_z = -1.e300,
    d_log_vol = log((x@n_points + 1) / x@n_points),
    tot_call = 0L,
    worst_lik = -1.e300,
    sampler_updates = 0L,
    n_since_update = 0L
  )

  env_bind(
    x@wrk,
    units = list(),
    points = list(),
    s_log_lik = list(),
    s_log_vol = list(),
    idx = list(),
    parent = list(),
    calls = list()
  )

  time1 <- Sys.time()
  sampler <- nested_sampling_impl(x, max_it, max_call, dlogz)

  # Cleanup
  x@wrk$time <- as.difftime(Sys.time() - time1, units = "secs")
  ErnestRun(sampler)
}

#' The nested sampling loop.
#'
#' @param sampler The `nested_sampler` object.
#' @param max_it The maximum number of iterations needed to run the sampler.
#' @param max_call Maximum number of calls to the likelihood function.
#' @param dlogz The threshold for the remaining prior volume to the total.
#'
#' @noRd
nested_sampling_impl <- function(sampler, max_it, max_call, dlogz) {
  status <- "RUN"
  iter <- 0L
  env <- sampler@wrk

  # cli::cli_progress_bar("Running Nested Sampler")
  for (iter in c(1:max_it)) {
    if (env$tot_call > max_call) {
      status <- "MAX_CALL"
      # if (env$verbose) cli::cli_progress_done()
      break
    }
    d_log_z <- logaddexp(0, max(env$live_lik) + env$log_vol - env$log_z)
    if (d_log_z < dlogz) {
      status <- "MIN_EVID"
      # if (env$verbose) cli::cli_progress_done()
      break
    }

    # Get the worst point and constrict the prior volume
    worst_idx <- which.min(env$live_lik)
    new_worst_lik <- env$live_lik[worst_idx]
    env$log_vol <- env$log_vol - env$d_log_vol

    # Update the integration
    log_d_vol <- log(0.5 * expm1(env$d_log_vol)) + env$log_vol
    log_wt <- logaddexp(new_worst_lik, env$worst_lik) + log_d_vol
    env$log_z <- logaddexp(env$log_z, log_wt)
    env$worst_lik <- new_worst_lik

    # Increment the sampler if required
    if (env$sampler_updates == 0L && env$tot_call > sampler@first_update) {
      env$sampler_updates <- 1L
      env$n_since_update <- 0L
      sampler <- update_sampler(sampler, env$live_points) %||% sampler
    }
    if (env$n_since_update > sampler@between_update) {
      sampler <- update_sampler(sampler, env$live_points) %||% sampler
      env$n_since_update <- 0L
      env$sampler_updates <- env$sampler_updates + 1L
    }

    # Save the updates
    env$units[[iter]] <- env$live_units[worst_idx, ]
    env$points[[iter]] <- env$live_points[worst_idx, ]
    env$s_log_lik[[iter]] <- env$worst_lik
    env$s_log_vol[[iter]] <- env$log_vol
    env$idx[[iter]] <- worst_idx
    env$sampler_iter[[iter]] <- env$sampler_updates

    # Evolve and replace the worst point
    copy <- sample.int(sampler@n_points, size = 1)
    while (copy == worst_idx && sampler@n_points != 1) {
      copy <- sample.int(sampler@n_points, size = 1)
    }
    new <- if (env$sampler_updates == 0L) {
      propose_uniform(sampler)
    } else {
      propose_live(sampler, copy)
    }
    env$n_since_update <- env$n_since_update + new$num_calls
    env$live_units[worst_idx, ] <- new$unit
    env$live_points[worst_idx, ] <- new$parameter
    env$live_lik[worst_idx] <- new$log_lik
    env$calls[[iter]] <- new$num_calls
    env$tot_call <- env$tot_call + new$num_calls
    env$parent[[iter]] <- copy
  }
  if (status == "RUN") {
    status <- "MAX_IT"
  }
  env$status <- status
  env$tot_it <- iter
  sampler
}


