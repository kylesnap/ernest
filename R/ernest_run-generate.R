#' @importFrom generics generate
#' @export
generics::generate

#' Run the nested sampler to generate new samples.
#'
#' @param maxit Maximum number of iterations to run the sampler. Must
#' represent a positive integer.
#' @param maxcall Maximum number of calls to the likelihood function. Must
#' represent a positive integer.
#' @param dlogz The threshold for the remaining prior volume to the total
#' evidence. Must represent a number larger or equal to zero.
#'
#' @return The `ErnestRun` object containing the results of the nested
#' sampling run.
S7::method(generate, ErnestRun) <- function(x, max_it = Inf, max_call = Inf, dlogz = 0.05) {
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
  if (env_cache(x @ env, "tot_it", 0L) != 0) {
    cli::cli_abort("The run has already been generated. ernest can't support a rerun yet.")
  }

  env_cache(x @ env, "log_vol", 0)
  env_cache(x @ env, "log_z", -1.e300)
  env_cache(x @ env, "d_log_vol", log((x@n_points + 1.) / x@n_points))
  env_cache(x @ env, "tot_call", 0L)
  env_cache(x @ env, "worst_lik", -1.e300)
  env_cache(x @ env, "sampler_updates", 0L)
  env_cache(x @ env, "n_since_update", 0L)

  env_cache(x @ env, "units", list())
  env_cache(x @ env, "points", list())
  env_cache(x @ env, "s_log_lik", numeric())
  env_cache(x @ env, "s_log_vol", numeric())
  env_cache(x @ env, "idx", numeric())
  env_cache(x @ env, "parent", numeric())
  env_cache(x @ env, "calls", numeric())
  env_cache(x @ env, "sampler_iter", numeric())

  env_bind(x@env, "sampler" = x@sampler)
  time1 <- Sys.time()
  status <- nested_sampling_impl(
    x@env,
    x@n_points,
    x@n_uniform,
    x@update_int,
    max_it,
    max_call,
    dlogz
  )
  env_poke(x @ env, "time", as.difftime(Sys.time() - time1, units = "secs"))

  # Cleanup
  x@sampler <- env_get(x@env, "sampler")
  env_unbind(x@env, "sampler")
  clean_workspace(x@env)
  invisible(x)
}

nested_sampling_impl <- function(env, n_points, n_uniform, update_int, max_it, max_call, dlogz) {
  status <- "RUN"
  iter <- 0L
  for (iter in c(1:max_it)) {
    if (env$tot_call > max_call) {
      status <- "MAX_CALL"
      #if (env$verbose) cli::cli_progress_done()
      break
    }
    d_log_z <- logaddexp(0, max(env$live_lik) + env$log_vol - env$log_z)
    if (d_log_z < dlogz) {
      status <- "MIN_EVID"
      #if (env$verbose) cli::cli_progress_done()
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
    if (env$sampler_updates == 0L && env$tot_call > n_uniform) {
      env$sampler_updates <- 1L
      env$n_since_update <- 0L
      env$sampler <- update_sampler(env$sampler, env$live_points) %||% env$sampler
    }
    if (env$n_since_update > update_int) {
      env$sampler <- update_sampler(env$sampler, env$live_points) %||% env$sampler
      env$n_since_update <- 0L
      env$sampler_updates <- env$sampler_updates + 1L
    }

    # Save the updates
    env$units[[length(units) + 1]] <- env$live_units[worst_idx, ]
    env$points[[length(points) + 1]] <- env$live_points[worst_idx, ]
    env$s_log_lik <- append(env$s_log_lik, env$worst_lik)
    env$s_log_vol <- append(env$s_log_vol, env$log_vol)
    env$idx <- append(env$idx, worst_idx)
    env$sampler_iter <- append(env$sampler_iter, env$sampler_updates)

    # Evolve and replace the worst point
    copy <- sample.int(n_points, size = 1)
    while (copy == worst_idx && n_points != 1) {
      copy <- sample.int(n_points, size = 1)
    }
    new <- if (env$sampler_updates == 0L) {
      propose_uniform(env$sampler, env$worst_lik)
    } else {
      propose_live(env$sampler, env$live_units[copy, ], env$worst_lik)
    }
    env$n_since_update <- env$n_since_update + new$num_calls
    env$live_units[worst_idx, ] <- new$unit
    env$live_points[worst_idx, ] <- new$parameter
    env$live_lik[worst_idx] <- new$log_lik
    env$calls <- append(env$calls, new$num_calls)
    env$tot_call <- env$tot_call + new$num_calls
    env$parent <- append(env$parent, copy)
  }
  if (status == "RUN") {
    status <- "MAX_IT"
  }
  env$tot_it <- env$tot_it + iter
  status
}

clean_workspace <- function(env) {
  env$history <- vctrs::new_rcrd(
    fields = list(
      "log_lik" = list_c(env$s_log_lik),
      "log_vol" = list_c(env$s_log_vol),
      "id" = env$idx,
      "parent" = env$parent,
      "calls" = env$calls,
      "sampler" = env$sampler_iter
    ),
    units = do.call(rbind, env$units),
    points = do.call(rbind, env$points)
  )
  env_unbind(
    env,
    c("s_log_lik", "s_log_vol", "idx", "parent", "calls", "sampler_iter",
      "units", "points")
  )
}
