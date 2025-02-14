#' An S7 Class, Containing a Nested Sampling Run
#' @include sampler.R
ErnestRun <- S7::new_class(
  "ErnestRun",
  properties = list(
    sampler = Sampler,
    n_points = S7::class_integer,
    n_uniform = S7::class_integer,
    update_int = S7::class_integer,
    verbose = S7::class_logical,
    n_iter = S7::new_property(
      getter = function(self) {
        env_cache(self %@% env, "tot_it", 0L)
      }
    ),
    n_call = S7::new_property(
      getter = function(self) {
        env_cache(self %@% env, "tot_call", 0L)
      }
    ),
    env = S7::class_environment
  ),
  constructor = function(sampler, n_points, n_uniform, update_int, verbose) {
    env <- new_environment()
    env$live_units <- matrix(
      stats::runif(sampler@num_dim * n_points),
      nrow = n_points,
      ncol = sampler@num_dim
    )
    env$live_points <- matrix(nrow = n_points, ncol = sampler@num_dim)
    env$live_lik <- numeric(n_points)

    for (i in 1:n_points) {
      env$live_points[i, ] <- sampler@prior_transform(env$live_units[i, ])
      env$live_lik[i] <- sampler@log_lik(env$live_points[i, ])
    }

    S7::new_object(
      S7::S7_object(),
      sampler = sampler,
      n_points = n_points,
      n_uniform = n_uniform,
      update_int = update_int,
      verbose = verbose,
      env = env
    )
  }
)
