#' Parallelization in ernest
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Nested sampling runs can be performed in parallel using \CRANpkg{mirai}.
#'
#' To parallelize a run:
#' - Create a portable likelihood with `parallel_likelihood()`.
#' - Create a portable prior with `parallel_prior()` or use one of ernest's
#' [special priors][special_priors].
#' - Set worker daemons with `mirai::daemons()` and ensure workers can load
#' ernest.
#' - Call [`generate()`][generate-ernest] with `parallel` != `FALSE`.
#'
#' @param scalar_fn,point_fn,vectorized_fn `[function]`\cr Used identically to
#' [create_likelihood()] or [create_prior()], but with the additional
#' requirement that the functions are 'fresh', meaning they should be declared
#' in the call to `parallel_likelihood()` or `parallel_prior()` rather than
#' stored in a variable.
#' @param ... Named arguments that should be captured in the function's
#' environment and serialized for remote execution.
#' @inheritParams create_prior
#' @inheritParams create_likelihood
#'
#' @returns
#' For `parallel_likelihood()`, a [carrier::crate] with the additional class
#' [ernest_likelihood].
#'
#' For `parallel_prior()`, an [ernest_prior] whose transformation function is
#' a [carrier::crate].
#'
#' @section Parallelized nested sampling:
#' Splitting an initial live set into subruns lets you use many live points
#' without running a single very large job. Run several smaller nested sampling
#' jobs concurrently and merge their records to obtain the same statistical
#' benefits as a single large run.
#'
#' When [`generate()`][generate-ernest] is called with `parallel != FALSE`, the
#' resulting run contains a `.parallel` element with per-worker summaries (see
#' [glance.ernest_run()]). Note that each worker must have at least `nvar * 2`
#' live points, where `nvar` is the number of variables in the parameter space.
#'
#' @section Daemons:
#' How parallelization occurs is determined by [mirai::daemons()]. Daemons must
#' be set prior to calling `generate()` with `parallel != FALSE`, otherwise an
#' error will be thrown.
#'
#' It is usual to set daemons once per session. You can leave them running on
#' your local machine as they consume almost no resources whilst waiting to
#' receive tasks. The following sets up 6 daemons locally:
#'
#' ```r
#' mirai::daemons(6)
#' ```
#'
#' @seealso [mirai::daemons()] [carrier::crate()] [generate-ernest]
#'
#' @references Documentation adapted from the \CRANpkg{mirai},
#' \CRANpkg{carrier}, and \CRANpkg{purrr} packages.
#'
#' @rdname parallelization
#' @export
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
    "scalar_fn" = crate_fn(substitute(scalar_fn), ...),
    "vectorized_fn" = crate_fn(substitute(vectorized_fn), ...)
  )
  new_ernest_likelihood(
    fn,
    interface = interface,
    on_nonfinite = .on_nonfinite
  )
}

#' @rdname parallelization
#' @export
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
    "point_fn" = crate_fn(substitute(point_fn), ...),
    "vectorized_fn" = crate_fn(substitute(vectorized_fn), ...)
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

#' Wrap `fn` in a crate and attach it to the search path
#'
#' @param arg The defused argument to be wrapped in a crate.
#' @param ... Additional arguments to be captured in the crate's environment.
#' @param .error_arg The argument to be reported in error messages.
#' @returns A crate.
#' @noRd
crate_fn <- function(
  arg,
  ...,
  .error_arg = caller_arg(arg),
  .error_call = caller_env()
) {
  inject(
    carrier::crate(
      !!arg,
      !!!list(...),
      .parent_env = global_env(),
      .error_arg = .error_arg,
      .error_call = .error_call
    )
  )
}

#' Run nested sampling in parallel across multiple daemons
#'
#' @param x The ernest_sampler object
#' @param control parameters for the nested sampling run, generated from
#' `set_run_control()`.
#' @param show_progress Logical. If `TRUE`, displays a progress bar during
#' sampling.
#' @param parallel Logical or integer. The number of parallel workers to use,
#' or `TRUE` to use all available daemons.
#'
#' @returns The output of `nested_sampling_impl()`, plus a `parallel` element
#' containing per-worker glances.
#' @noRd
nested_sampling_parallel <- function(
  x,
  control,
  show_progress,
  parallel
) {
  check_parallel_enabled(x, call = caller_env())
  parallel <- if (isTRUE(parallel)) {
    mirai::info()[["connections"]]
  } else {
    parallel
  }
  live_env <- x$live_env
  ids <- env_get(live_env, "id")
  nlive <- x$nlive

  if (show_progress) {
    cli::cli_progress_step("Splitting live set...")
  }
  ids_by_daemon <- allocate_nlive(ids, parallel, attr(x$prior, "nvar"))
  parallel_runs <- partition_run(
    live_env,
    ids_by_daemon,
    control,
    x$rcrd
  )

  if (show_progress) {
    cli::cli_progress_step(
      "Performing {length(parallel_runs)} runs in parallel..."
    )
  }
  # Run runs in parallel
  m <- mirai::mirai_map(
    parallel_runs,
    \(run) {
      cur_env <- list2env(run[c("unit", "log_lik", "birth_lik", "id")])
      nested_sampling_impl_(
        live_env = cur_env,
        lrps = lrps_,
        control = run$control,
        show_progress = FALSE
      )
    },
    nested_sampling_impl_ = nested_sampling_impl,
    lrps_ = x$lrps
  )
  m_out <- mirai::collect_mirai(m, options = ".stop")
  if (show_progress) {
    cli::cli_progress_step("Merging runs together...")
  }
  combined <- unpartition_runs(m_out, nlive)
  list("results" = combined$rcrd, ".parallel" = combined$glance)
}

#' Validate parallel configuration and sampler portability
#'
#' Ensures `sampler` contains portable `crate`d functions, and that `mirai`
#' daemons are available.
#'
#' @noRd
runs_in_parallel <- function(sampler) {
  is_installed(c("mirai", "carrier")) &&
    inherits(sampler$log_lik, "crate") &&
    !inherits(sampler$prior, "custom_prior") &&
    mirai::daemons_set()
}

#' Load ernest and an LRPS into the daemons
#'
#' @param lrps An ernest_lrps object.
#' @param call The calling environment.
#'
#' @returns TRUE if successful; else fails with a message.
#' @noRd
load_daemons <- function(lrps, load_all = TRUE, call = caller_env()) {
  m <- if (isTRUE(load_all)) {
    mirai::everywhere(devtools::load_all("~/Projects/ernest"), lrps__ = lrps)
  } else {
    mirai::everywhere(rm(lrps__))
    mirai::everywhere({}, lrps__ = lrps)
  }
  first_fail <- match(TRUE, vapply(m[], mirai::is_error_value, logical(1)))
  if (!is.na(first_fail)) {
    cnd <- m[][[first_fail]]
    cli::cli_abort(
      c(
        "Can't initialize daemons for nested sampling.",
        ">" = "Error: {.str {cnd$message}}"
      ),
      body = cnd$body,
      trace = cnd$trace,
      call = call
    )
  }
  invisible(TRUE)
}

#' Ensure parallel packages are available
#'
#' Internal helper that validates required packages for parallel execution.
#'
#' @noRd
check_parallel_libs <- function(call = caller_env()) {
  check_installed(
    c("mirai", "carrier"),
    reason = "to run parallel nested sampling",
    call = call
  )
}
