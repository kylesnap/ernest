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

#' Perform parallel nested sampling
#'
#' @param x An ernest_sampler/ernest_run object.
#' @param workers Either `TRUE` to automatically use all available daemons,
#' or a vector of integers specifying the number of live points to allocate to
#' each daemon.
#' @param info,control Arguments controlling the parent run.
#' @param show_progress If `TRUE`, a progress bar will be displayed for the
#' parallel execution.
#'
#' @returns Same as `generate()`, but with the `parallel` element of the output
#' containing per-worker glance summaries.
#'
#' @noRd
pgenerate <- function(
  x,
  workers,
  info,
  control,
  show_progress,
  call = caller_env()
) {
  check_parallel_enabled(x, call)
  daemon_nlive <- if (isTRUE(workers)) {
    default_daemon_nlive(x$nlive, x$lrps$nvar, call = call)
  } else {
    vctrs::vec_cast(workers, integer())
  }
  nvar <- as.integer(x$lrps$nvar)
  if (vctrs::vec_any_missing(daemon_nlive) || any(daemon_nlive < (nvar + 1L))) {
    stop_input_type(
      workers,
      c("`TRUE`/`FALSE`", "a vector of integers each >= nvar + 1"),
      call = call
    )
  }
  tot_nlive <- sum(daemon_nlive) %|% 0L
  if (tot_nlive != x$nlive) {
    stop_input_type(
      workers,
      c("`TRUE`/`FALSE`", "a vector of integers summing to `x$nlive`"),
      call = call
    )
  }

  # Break apart the IDS into the appropriate number of workers
  ids <- vctrs::vec_chop(sample.int(x$nlive), sizes = daemon_nlive)
  split_x <- partition_run(x, ids, control, info)
  prev_rcrd <- if (is.null(x$rcrd)) {
    NULL
  } else {
    x$rcrd[vctrs::vec_as_location(field(x$rcrd, "neval") != 0L, length(x$rcrd))]
  }

  # Run the workers in parallel
  lrps_ <- x$lrps
  m <- mirai::mirai_map(
    split_x,
    \(xi) {
      cur_env <- list2env(xi[c("unit", "log_lik", "birth_lik")])
      nested_sampling_impl_(
        live_env = cur_env,
        lrps = lrps_,
        sampler_info = xi$info,
        control = xi$control,
        show_progress = FALSE
      )
    },
    nested_sampling_impl_ = nested_sampling_impl,
    lrps_ = x$lrps
  )

  opts <- c(".stop", if (show_progress) ".progress" else NULL)
  m_out <- mirai::collect_mirai(m, options = opts)
  combined <- unpartition_runs(m_out, ids, prev_rcrd)
  new_ernest_run(x, combined$rcrd, parallel = combined$glance)
}

#' Default number of live points per daemon
#'
#' @param nlive Total number of live points.
#' @param nvar Number of variables in the problem.
#' @param ndaemons Number of daemons available. If `NULL`, the number of daemons
#' will be obtained from `mirai::info()` (used for testing).
#' @param call The calling environment.
#'
#' @returns An integer vector of length `ndaemons` specifying the number of live
#' points to allocate to each daemon.
#'
#' @noRd
default_daemon_nlive <- function(
  nlive,
  nvar,
  ndaemons = NULL,
  call = caller_env()
) {
  ndaemons <- ndaemons %||%
    {
      mirai::require_daemons(call = call)
      mirai::info()[["connections"]]
    }
  nlive_per_daemon <- nlive %/% ndaemons
  if (nlive_per_daemon < (nvar * 2L)) {
    nlive_per_daemon <- nvar * 2L
    ndaemons <- nlive %/% nlive_per_daemon
    cli::cli_warn(
      "Initializing {nlive_per_daemon} live points in {ndaemons} daemon{?s}.",
      call = call
    )
  }
  daemon_nlive <- rep(nlive_per_daemon, ndaemons)
  daemon_nlive[[1]] <- daemon_nlive[[1]] + (nlive - sum(daemon_nlive))
  vec_cast(daemon_nlive, integer())
}

#' Partition a run for parallel execution
#'
#' @param x An ernest_sampler/ernest_run object.
#' @param slices A list of integer vectors specifying the indices of the live
#' points to allocate to each worker.
#' @param control,info Arguments controlling the parent run.
#'
#' @returns A list of lists containing the data and control parameters for each
#' worker.
#' @noRd
partition_run <- function(x, slices, control, info) {
  x_rcrd <- x$rcrd %||% NULL
  split_info <- function(split_nlive) {
    frac_nlive <- split_nlive / info$nlive
    list(
      seed = info$seed,
      nlive = split_nlive,
      first_update = as.integer(info$first_update * frac_nlive),
      update_interval = as.integer(info$update_interval * frac_nlive)
    )
  }

  lapply(slices, \(slice) {
    list(
      "unit" = env_get(x$live_env, "unit")[slice, , drop = FALSE],
      "log_lik" = env_get(x$live_env, "log_lik")[slice],
      "birth_lik" = env_get(x$live_env, "birth_lik")[slice],
      "info" = split_info(length(slice)),
      "control" = new_generate_control(
        control$max_iterations,
        control$max_evaluations,
        control$min_logz,
        prev_run = if (!is.null(x_rcrd)) {
          vctrs::vec_slice(x_rcrd, field(x_rcrd, "id") %in% slice)
        }
      )
    )
  })
}

#' Recombine per-worker records into a single run
#'
#' @param m_out A list of worker outputs, each containing a record of the
#' worker's nested sampling run.
#' @param ids A list of integer vectors specifying the indices of the live
#' points allocated to each worker.
#' @param prev_rcrd The record of the parent run, if it exists.
#'
#' @returns A list containing the combined rcrd and a glance summary of the
#' each worker's run.
#' @noRd
unpartition_runs <- function(m_out, ids, prev_rcrd) {
  run_rcrds <- lapply(
    seq_along(m_out),
    \(i) {
      if (is.null(prev_rcrd)) {
        return(m_out[[i]])
      }
      prev_i <- prev_rcrd[field(prev_rcrd, "id") %in% ids[[i]]]
      c(prev_i, m_out[[i]])
    }
  )
  glance_df <- vctrs::vec_c(!!!lapply(run_rcrds, glance))
  acc <- NULL
  for (i in seq_along(m_out)) {
    acc <- if (is.null(acc)) {
      run_rcrds[[i]]
    } else {
      merge_rcrd(acc, run_rcrds[[i]], keep = "all")
    }
  }
  list("rcrd" = acc, "glance" = glance_df)
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
