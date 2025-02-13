# Find the worst point in a nested sample run and mark it for replacement.
ErnestRun$set("private", ".pop", function() {
  # Get the worst point and constrict the prior volume
  private$.worst_idx <- which.min(private$.live_lik)
  new_worst_lik <- private$.live_lik[private$.worst_idx]
  private$.log_vol <- private$.log_vol - private$.d_log_vol

  # Update the integration
  log_d_vol <- log(0.5 * expm1(private$.d_log_vol)) + private$.log_vol
  log_wt <- logaddexp(new_worst_lik, private$.worst_lik) + log_d_vol
  private$.log_z <- logaddexp(private$.log_z, log_wt)
  private$.worst_lik <- new_worst_lik

  # Increment the sampler if required
  if (private$.sampler_updates == 0L && private$.total_calls > private$.n_uniform) {
    private$.sampler_updates <- 1L
    private$.n_since_update <- 0L
    private$.sampler <- update_sampler(private$.sampler, private$.live_points) %||% private$.sampler
  }

  if ((private$.n_since_update > private$.update_int) %|% FALSE) {
    private$.sampler <- update_sampler(private$.sampler, private$.live_points) %||% private$.sampler
    private$.n_since_update <- 0L
    private$.sampler_updates <- private$.sampler_updates + 1L
  }

  # Save the updates
  private$.units[[length(private$.units) + 1]] <-
    private$.live_units[private$.worst_idx, ]
  private$.points[[length(private$.points) + 1]] <-
    private$.live_points[private$.worst_idx, ]
  private$.s_log_lik <- append(private$.s_log_lik, private$.worst_lik)
  private$.s_log_vol <- append(private$.s_log_vol, private$.log_vol)
  private$.idx <- append(private$.idx, private$.worst_idx)
})

# Evolve and replace the worst point in a nested sampling run
ErnestRun$set("private", ".push", function() {
  copy <- sample.int(private$.num_points, size = 1)
  while (copy == private$.worst_idx && private$.num_points != 1) {
    copy <- sample.int(private$.num_points, size = 1)
  }
  new <- if (private$.sampler_updates == 0L) {
    propose_uniform(private$.sampler, private$.worst_lik)
  } else {
    propose_live(private$.sampler, private$.live_units[copy, ], private$.worst_lik)
  }
  private$.n_since_update <- private$.n_since_update + new$num_calls
  private$.live_units[private$.worst_idx, ] <- new$unit
  private$.live_points[private$.worst_idx, ] <- new$parameter
  private$.live_lik[private$.worst_idx] <- new$log_lik
  private$.calls <- append(private$.calls, new$num_calls)
  private$.total_calls <- private$.total_calls + new$num_calls
  private$.parent <- append(private$.parent, copy)
})
