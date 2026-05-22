parallel_likelihood <- function(
  scalar_fn,
  vectorized_fn,
  ...,
  .on_nonfinite = c("warn", "quiet", "abort")
) {
  check_parallel_libs()
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
  check_parallel_libs()
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
    .repair = .repair,
    .class = "crated_prior"
  )
}

check_parallel_enabled <- function(sampler, call = caller_env()) {
  check_parallel_libs(call = call)
  if (!inherits(sampler$log_lik, "crate")) {
    cli::cli_abort(
      c(
        "`{caller_arg(sampler)}` must contain a portable `log_lik` function.",
        "i" = "Did you forget to use {.fn ernest::parallel_likelihood}?"
      ),
      call = call
    )
  }
  safe_priors <- c("crated_prior", "normal_prior", "uniform_prior")
  if (!inherits_any(sampler$prior, safe_priors)) {
    cli::cli_abort(
      c(
        "`{caller_arg(sampler)}` must contain a portable `prior` function.",
        "i" = "Did you forget to use {.fn ernest::parallel_prior}?"
      ),
      call = call
    )
  }
  mirai::require_daemons(call = call)
  m <- mirai::everywhere(library(ernest))
  first_fail <- match(TRUE, vapply(m[], mirai::is_error_value, logical(1)))
  if (!is.na(first_fail)) {
    cli::cli_abort(
      c(
        "{.pkg mirai} threw an error when trying to load {.pkg ernest}.",
        "x" = "{m[][[first_fail]]}"
      ),
      call = call
    )
  }
  invisible(NULL)
}

check_parallel_libs <- function(call = caller_env()) {
  check_installed(
    c("mirai", "carrier"),
    reason = "to run parallel nested sampling",
    call = call
  )
}
