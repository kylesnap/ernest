#' @param x The ernest_sampler or ernest_run object.
#' @param sampler_info A list containing information about the sampler.
#' @param control parameters for the nested sampling run, generated from
#' `set_run_control()`.
#' @param show_progress Logical. If `TRUE`, displays a progress bar during
#' sampling.
#'
#' @noRd
p_generate <- function(
  x,
  sampler_info,
  run_control,
  show_progress,
  allow_par,
  call = caller_env()
) {
  check_installed("mirai", "allow_par", call = call)
  mirai::require_daemons(call = call)
  nlive <- x$nlive
  hint <- NULL
  workers <- if (isTRUE(allow_par)) {
    hint <- "Have you changed the number of daemons set by {.pkg mirai}?"
    rep(na_int, mirai::info()[["connections"]])
  } else {
    hint <- "Have you changed how the run is parallelized with `allow_par`?"
    vctrs::vec_cast(allow_par, integer(), call = call)
  }
  nlive_threaded <- thread_nlive(nlive, workers, call)
  thread_envs <- split_live(x$live_env, nlive_threaded, sampler_info, call)

  m <- mirai::mirai_map(
    thread_envs$live,
    \(thread) {
      fn(
        thread$env,
        lrps = l,
        thread$info,
        control = rc,
        show_progress = FALSE
      )
    },
    fn = nested_sampling_impl,
    l = x$lrps,
    rc = run_control
  )

  results <- mirai::collect_mirai(
    m,
    options = c(".stop", if (show_progress) ".progress" else NULL)
  )
  result <- collect_results(results, thread_envs$split)
  collect_live(x$live_env, thread_envs$live, thread_envs$split)
  new_ernest_run(x, result)
}

#' Get the nlive for each worker.
#'
#' @param nlive The total nlive of the sampler.
#' @param workers An integer vector containing nlive per worker or NA.
#' @param hint A string printed when an error is thrown.
#' @param call Calling environment for errors.
#'
#' @returns A validated integer vector, whose length is the number of
#' runs and each element is that run's nlive.
#' @noRd
thread_nlive <- function(nlive, workers, hint, call = caller_env()) {
  if (vctrs::vec_is_empty(workers)) {
    cli::cli_abort(
      c("At least one worker must be specified.", "i" = hint),
      call = call
    )
  }
  workers <- if (all(is.na(workers))) {
    rep(nlive %/% length(workers), length(workers))
  } else if (any(!is.finite(workers)) || any(workers < 1L)) {
    cli::cli_abort(
      c("parallel runs must each contain at least one live point", "i" = hint),
      call = call
    )
  } else {
    workers
  }
  if (sum(workers) > nlive) {
    cli::cli_abort(
      c(
        "parallel runs must contain a total of {nlive} live points",
        "i" = hint
      ),
      call = call
    )
  }
  workers[[1]] <- workers[[1]] + (nlive - sum(workers))
  as.integer(workers)
}

#' Split the live set into a series of threads.
#'
#' @param live_env The original live set, already validated.
#' @param workers Integer vector describing how points are to be
#' allocated
#' @param info Sampler info, which is broken up into per-thread control lists.
#' @param call Error info.
#'
#' @returns Named list of IDs by worker and environments.
#' @noRd
split_live <- function(live_env, workers, info, call = caller_env()) {
  withr::local_preserve_seed()
  nwork <- length(workers)
  idx_split <- sample.int(info$nlive, info$nlive, replace = FALSE)
  splits <- .mapply(
    \(s, e) vctrs::vec_as_location(sort(idx_split[s:e]), n = info$nlive),
    dots = list(
      s = c(1, cumsum(workers)[-nwork] + 1),
      e = cumsum(workers)
    ),
    MoreArgs = NULL
  )
  live <- lapply(splits, \(spl) {
    list(
      "env" = new_environment(
        data = list(
          "unit" = env_get(live_env, "unit")[spl, , drop = FALSE],
          "log_lik" = env_get(live_env, "log_lik")[spl],
          "birth_lik" = env_get(live_env, "birth_lik")[spl]
        )
      ),
      "info" = split_info(spl, info)
    )
  })
  list("split" = splits, "live" = live)
  # TODO: Run check_live_set here?
}

split_info <- function(spl, info) {
  thread_nlive <- length(spl)
  frac_nlive <- thread_nlive / info$nlive
  list(
    seed = info$seed,
    nlive = thread_nlive,
    first_update = as.integer(info$first_update * frac_nlive),
    update_interval = as.integer(info$update_interval * frac_nlive)
  )
}

collect_results <- function(results, split) {
  results <- .mapply(
    \(lst, spl) {
      lst$dead_id <- spl[list_c(lst$dead_id)]
      lst
    },
    dots = list(lst = results, spl = split),
    MoreArgs = NULL
  )
  log_lik <- do.call(c, lapply(results, `[[`, "dead_log_lik"))
  ordered_lik <- order(log_lik)
  bind_all <- \(nm) do.call(c, lapply(results, `[[`, nm))[ordered_lik]
  list(
    "dead_unit" = bind_all("dead_unit"),
    "dead_log_lik" = bind_all("dead_log_lik"),
    "dead_id" = vctrs::list_of(bind_all("dead_id"), integer()),
    "dead_evals" = bind_all("dead_evals"),
    "dead_birth" = bind_all("dead_birth")
  )
}

collect_live <- function(live_env, thread_envs, splits) {
  mapply(
    \(t_env, spl) {
      live_env$unit[spl, ] <- env_get(t_env, "unit")
      live_env$log_lik[spl] <- env_get(t_env, "log_lik")
      live_env$birth_lik[spl] <- env_get(t_env, "birth_lik")
      invisible(env_unbind(t_env, c("unit", "log_lik", "birth_lik")))
    }
  )
  live_env
}
