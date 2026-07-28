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
  check_parallel_enabled(x)
  if (isTRUE(parallel)) {
    parallel <- mirai::info()[["connections"]]
  }
  check_number_whole(parallel, min = 1)

  # Divide the live set into subruns
  allocations <- allocate_nlive(x$nlive, parallel, attr(x$prior, "nvar"))
  parallel_runs <- partition_run(
    x$live_env,
    allocations,
    control,
    rcrd = x$rcrd
  )

  if (show_progress) {
    nruns <- length(allocations)
    sub_nlive <- vctrs::list_sizes(allocations[[length(allocations)]])
    cli::cli_progress_step(
      "Performing {nruns} run{?s} with at least {sub_nlive} live point{?s}...",
      spinner = TRUE
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
  unpartition_runs(m_out, nlive = x$nlive)
}

#' Assign IDs to daemons for parallel nested sampling
#'
#' @param nlive The number of live points in the live set.
#' @param parallel The number of parallel workers to use.
#' @param nvar The number of variables in the parameter space.
#'
#' @returns A list of character vectors, where each vector contains the IDs
#' assigned to a daemon.
#'
#' @noRd
allocate_nlive <- function(nlive, parallel, nvar) {
  nlive_parallel <- nlive %/% parallel
  nlive_parallel_nvar <- nlive_parallel / nvar
  min_nlive_nvar <- getOption("ernest.min_nlive_nvar", 10L)
  if (nlive_parallel_nvar < 1L) {
    cli::cli_abort(c(
      "Must have at least one live point within each subrun.",
      "i" = "Should you lower `parallel` or raise `nlive`?"
    ))
  } else if (nlive_parallel_nvar < min_nlive_nvar) {
    new_parallel <- nlive %/% (min_nlive_nvar * nvar)
    cli::cli_warn(c(
      "Automatically adjusting `parallel` from {parallel} to {new_parallel},",
      "ensuring each worker has {min_nlive_nvar} live points per variable.",
      "i" = "Override this with {.code getOption('ernest.min_nlive_nvar', 0)}."
    ))
    parallel <- new_parallel
    nlive_parallel <- nlive %/% new_parallel
  }
  sampled_idx <- sample.int(nlive, size = nlive, replace = FALSE)
  daemon_sizes <- rep.int(nlive_parallel, times = parallel)
  remaining_nlive <- seq_len(nlive %% nlive_parallel)
  daemon_sizes[remaining_nlive] <- daemon_sizes[remaining_nlive] + 1
  vctrs::vec_chop(sampled_idx, sizes = daemon_sizes)
}

#' Partitions a compiled sampler for parallel execution
#'
#' @param live_env The live set environment from a compiled sampler.
#' @param ids Character vectors of IDs.
#' @param control Arguments controlling the parent run.
#' @param rcrd An option ernest_rcrd with previously generated results.
#'
#' @returns A list of lists containing the data and control parameters for each
#' worker.
#' @noRd
partition_run <- function(live_env, alloc, control, rcrd = NULL) {
  lapply(
    alloc,
    \(idx) {
      run_info <- list(
        "unit" = env_get(live_env, "unit")[idx, , drop = FALSE],
        "log_lik" = env_get(live_env, "log_lik")[idx],
        "birth_lik" = env_get(live_env, "birth_lik")[idx],
        "id" = env_get(live_env, "id")[idx]
      )
      run_info$control <- if (!is.null(rcrd)) {
        dead_idx <- which(vctrs::vec_in(field(rcrd, "id"), run_info$id))
        generate_control(
          control$max_iterations,
          control$max_evaluations,
          control$min_logz,
          seed = control$seed,
          nlive = length(idx),
          refresh_frac = control$refresh_frac,
          rcrd = rcrd[dead_idx],
        )
      } else {
        generate_control(
          control$max_iterations,
          control$max_evaluations,
          control$min_logz,
          seed = control$seed,
          nlive = length(idx),
          refresh_frac = control$refresh_frac
        )
      }
      run_info
    }
  )
}

#' Recombine per-worker records into a single run
#'
#' @param m_out A list of worker outputs, each containing a record of the
#' worker's nested sampling run.
#' @param nlive The total number of live points.
#'
#' @returns A list containing the combined rcrd and a glance summary of the
#' each worker's run.
#' @noRd
unpartition_runs <- function(m_out, nlive) {
  glance_df <- vctrs::vec_c(!!!lapply(m_out, glance))
  merged_rcrd <- unchop_rcrds(m_out, nlive = nlive)
  list("rcrd" = merged_rcrd, ".parallel" = glance_df)
}

#' Check if sampling can be performed in parallel
#'
#' @param sampler An ernest_sampler object.
#' @param call The calling environment, used for error reporting.
#'
#' @returns Invisibly returns `NULL` if parallelization is possible; otherwise,
#' throws an error.
#' @noRd
check_parallel_enabled <- function(
  sampler,
  call = caller_env(),
  arg = caller_arg(sampler)
) {
  check_parallel_libs(call = call)
  if (!inherits(sampler$log_lik, "crate")) {
    cli::cli_abort(
      "`{arg}` must contain a portable `log_lik` function.",
      class = "ernest_nonportable_sampler",
      call = call
    )
  }
  safe_priors <- c("crated_prior", "normal_prior", "uniform_prior")
  if (!inherits_any(sampler$prior, safe_priors)) {
    cli::cli_abort(
      "`{arg}` must contain a portable `prior` function.",
      class = "ernest_nonportable_sampler",
      call = call
    )
  }
  preserve_seed(attr(sampler, "seed"), .local_envir = call)
  mirai::require_daemons(call = call)
  load_ernest_on_daemons(call = call)
  invisible(NULL)
}

#' Load ernest onto all daemons (whether ernest is installed or in development)
#'
#' @param call The calling environment, used for error reporting.
#' @returns Invisibly returns `NULL` if ernest is successfully loaded onto all
#' daemons; otherwise, throws an error.
#' @noRd
load_ernest_on_daemons <- function(call = caller_env()) {
  load_expr <- if (
    is_installed("pkgload") && pkgload::is_dev_package("ernest")
  ) {
    cli::cli_warn(
      "Loading dev. version of {.pkg ernest} on daemons...",
      class = "ernest.on_dev"
    )
    expr(devtools::load_all(
      !!(pkgload::pkg_path(path = getOption("ernest.dev_path", default = ".")))
    ))
  } else {
    expr(library(ernest))
  }
  mirai::require_daemons(call = call)
  m <- mirai::everywhere(load_expr)
  first_fail <- match(TRUE, vapply(m[], mirai::is_error_value, logical(1)))
  if (!is.na(first_fail)) {
    fail <- m[[first_fail]]$data
    cli::cli_abort(
      c(
        "Couldn't load {.pkg ernest} onto all daemons.",
        "First failed on daemon #{first_fail}:"
      ),
      body = fail,
      trace = attr(fail, "stack.trace"),
      call = call
    )
  }
  invisible(NULL)
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
