parallel_likelihood <- function(
  scalar_fn,
  vectorized_fn,
  ...,
  .on_nonfinite = c("warn", "quiet", "abort")
) {
  check_parallel_support()
  interface <- check_exclusive(scalar_fn, vectorized_fn)
  fn <- switch(
    interface,
    "scalar_fn" = scalar_fn,
    "vectorized_fn" = vectorized_fn
  )
  fn <- carrier::crate(
    set_env(fn),
    !!!list2(...)
  )
  new_ernest_likelihood(
    fn,
    interface = interface,
    on_nonfinite = .on_nonfinite
  )
}

parallel_prior <- function(
  point_fn,
  vectorized_fn,
  ...,
  .names,
  .lower = -Inf,
  .upper = Inf,
  .repair = c(
    "unique",
    "universal",
    "check_unique",
    "unique_quiet",
    "universal_quiet"
  )
) {
  check_parallel_support()
  interface <- check_exclusive(point_fn, vectorized_fn)
  fn <- switch(
    interface,
    "point_fn" = point_fn,
    "vectorized_fn" = vectorized_fn
  )
  fn <- carrier::crate(
    set_env(fn),
    !!!list2(...)
  )
  new_ernest_prior(
    fn,
    names = .names,
    lower = .lower,
    upper = .upper,
    interface = interface,
    .repair = .repair
  )
}

check_parallel_support <- function() {
  check_installed(
    c("mirai", "carrier"),
    reason = "to run parallel nested sampling",
    call = caller_env()
  )
}
