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

pgenerate <- function(
  x,
  workers,
  info,
  control,
  show_progress,
  call = caller_env()
) {
  check_parallel_enabled(x, call)
  worker_nlive <- if (isTRUE(workers)) {
    default_worker_nlive(x$nlive, x$lrps$nvar)
  } else {
    vec_cast(workers, integer())
  }
  if (any(worker_nlive < x$ndim) || vctrs::vec_any_missing(worker_nlive)) {
    stop_input_type(
      workers,
      c("`TRUE`", "a vector of integers greater than or equal to `x$ndim`"),
      call = call
    )
  }
  tot_nlive <- sum(worker_nlive) %|% 0L
  if (tot_nlive != x$nlive) {
    stop_input_type(
      workers,
      c("`TRUE`", "a vector of integers summing to `x$nlive`"),
      call = call
    )
  }

  # Break apart the IDS into the appropriate number of workers
  ids <- vctrs::vec_chop(sample.int(x$nlive), sizes = worker_nlive)
  split_x <- partition_run(x, ids, info, control)
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
  rcrd <- unpartition_runs(m_out, ids, prev_rcrd)
  new_ernest_run(x, rcrd)
}

default_worker_nlive <- function(nlive, ndim, call = caller_env()) {
  ndaemons <- mirai::info()[["connections"]]
  nlive_per_worker <- nlive %/% ndaemons
  if (nlive_per_worker < (ndim + 1)) {
    cli::cli_warn(
      "The number of live points per daemon has been set to `ndim + 1`.",
      call = call
    )
    nlive_per_worker <- ndim + 1
  }
  workers <- rep(nlive_per_worker, ndaemons)
  workers[[1]] <- workers[[1]] + (nlive - sum(workers))
  vec_cast(workers, integer())
}

#' Split the live set into a series of threads.
#'
#' @param x The sampler.
#' @param List of integer vectors, each containing the IDs for that worker.
#'
#' @returns Named list of IDs by worker and environments.
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

#' Recombine the results of the threads into a single run.
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
  acc <- NULL
  for (i in seq_along(m_out)) {
    acc <- if (is.null(acc)) {
      run_rcrds[[i]]
    } else {
      merge_rcrd(acc, run_rcrds[[i]], keep = "first")
    }
  }
  acc
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
  # TODO: SWAP COMMENT
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

check_parallel_libs <- function(call = caller_env()) {
  check_installed(
    c("mirai", "carrier"),
    reason = "to run parallel nested sampling",
    call = call
  )
}
