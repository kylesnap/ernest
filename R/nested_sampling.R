#' Perform Nested Sampling
#'
#' @param x A function returning the log likelihood of a model given some parameters.
#' @param ... dots, babey
#' @param sampler The likelihood-restricted prior sampler to use.
#' @param verbose Whether to print progress updates to the terminal.
#' @param ... Ignored.
#'
#' @returns Lots of stuff about the run.
#' @export
nested_sampling <- function(x, num_points = 500L, n_uniform = 100L, update_int = 100L, verbose = FALSE) {
  run <- ErnestRun(
    sampler = x,
    n_points = num_points,
    n_uniform = n_uniform,
    update_int = update_int,
    verbose = verbose
  )
}

create_run_env <- function() {
  rlang::new_environment(
    list(
      live_units = matrix(),
      live_points = matrix(),
      live_lik = double(),
      total_calls = 0L,
      totit = 0L,
      worst_idx = 0L,
      worst_lik = -1.e300,
      d_log_vol = 0,
      log_vol = 0,
      log_z = -1.e300,
      time = as.difftime(0, units = "secs"),
      sampler_updates = 0L,
      n_since_update = NA,
      units = list(),
      points = list(),
      saved_log_lik = list(),
      saved_log_vol = list(),
      idx = list(),
      calls = list(),
      parent = list()
    )
  )
}
