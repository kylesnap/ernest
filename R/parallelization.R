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
#' When [`generate()`][generate-ernest] is called with `parallel != FALSE`, the resulting run
#' contains a `parallel` element with per-worker summaries (see
#' [glance.ernest_run()]) useful for diagnostics.
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
#' @param sampler_info A list containing information about the sampler.
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
  sampler_info,
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

  ids_by_daemon <- allocate_nlive(ids, parallel)
  parallel_runs <- partition_run(
    live_env,
    ids_by_daemon,
    control,
    sampler_info,
    x$rcrd
  )

  # Run runs in parallel
  m <- mirai::mirai_map(
    parallel_runs,
    \(run) {
      cur_env <- list2env(run[c("unit", "log_lik", "birth_lik", "id")])
      nested_sampling_impl_(
        live_env = cur_env,
        lrps = lrps_,
        sampler_info = run$info,
        control = run$control,
        show_progress = FALSE
      )
    },
    nested_sampling_impl_ = nested_sampling_impl,
    lrps_ = x$lrps
  )
  opts <- c(".stop", if (show_progress) ".progress" else NULL)
  m_out <- mirai::collect_mirai(m, options = opts)
  combined <- unpartition_runs(m_out, nlive)
  list("results" = combined$rcrd, ".parallel" = combined$glance)
}

#' Assign IDs to daemons for parallel nested sampling
#'
#' @param ids Character vector of live point IDs.
#' @param parallel Integer. The number of parallel workers to use.
#' @param call The calling environment, used for error reporting.
#'
#' @returns A list of character vectors, where each vector contains the IDs
#' assigned to a daemon.
#'
#' @noRd
allocate_nlive <- function(
  ids,
  parallel,
  call = caller_env()
) {
  check_number_whole(parallel, min = 1, call = call)
  nlive <- vctrs::vec_unique_count(ids)
  nlive_p_daemon <- max(
    nlive %/% parallel,
    getOption("ernest.parallel_min_nlive", 100L)
  )
  daemon_nlive <- rep.int(nlive_p_daemon, times = nlive %/% nlive_p_daemon)
  daemon_nlive[[1]] <- daemon_nlive[[1]] + (nlive - sum(daemon_nlive))
  vctrs::vec_chop(sample(ids), sizes = daemon_nlive)
}

#' Partition a run for parallel execution
#'
#' @param live_env An environment containing sampling information.
#' @param ids Character vectors of IDs.
#' @param control,info Arguments controlling the parent run.
#' @param rcrd ernest_rcrd from the previous run
#'
#' @returns A list of lists containing the data and control parameters for each
#' worker.
#' @noRd
partition_run <- function(live_env, ids, control, info, rcrd = NULL) {
  # Helper to scale the parent run's info for each worker
  split_info <- \(split_ids) {
    split_nlive <- length(split_ids)
    frac_nlive <- split_nlive / info$nlive
    list(
      seed = info$seed,
      nlive = split_nlive,
      first_update = as.integer(info$first_update * frac_nlive),
      update_interval = as.integer(info$update_interval * frac_nlive)
    )
  }

  # Helper to split rcrd into runs for each worker
  split_rcrd <- \(split_ids) {
    if (is.null(rcrd)) {
      return(NULL)
    }
    vctrs::vec_slice(rcrd, field(rcrd, "id") %in% split_ids)
  }

  lapply(ids, \(id_slice) {
    live_loc <- vctrs::vec_match(env_get(live_env, "id"), id_slice)
    list(
      "unit" = env_get(live_env, "unit")[live_loc, , drop = FALSE],
      "log_lik" = env_get(live_env, "log_lik")[live_loc],
      "birth_lik" = env_get(live_env, "birth_lik")[live_loc],
      "id" = env_get(live_env, "id")[live_loc],
      "info" = split_info(id_slice),
      "control" = new_generate_control(
        control$max_iterations,
        control$max_evaluations,
        control$min_logz,
        prev_run = split_rcrd(id_slice)
      )
    )
  })
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
  merged_rcrd <- vctrs::vec_c(!!!m_out) |> sort()
  merged_rcrd <- compile_merged_rcrd(merged_rcrd, nlive)
  list("rcrd" = merged_rcrd, "glance" = glance_df)
}

#' Validate parallel configuration and sampler portability
#'
#' Ensures `sampler` contains portable `crate`d functions, and that `mirai`
#' daemons are available.
#'
#' @noRd
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
  # TODO: SWAP TO LIBRARY
  print("USING LOADALL")
  m <- mirai::everywhere(devtools::load_all("~/Projects/ernest"))
  # m <- mirai::everywhere(library(ernest))
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
