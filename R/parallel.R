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
  workers <- if (isTRUE(allow_par)) {
    hint <- "Have you changed the number of daemons set by {.pkg mirai}?"
    rep(na_int, mirai::info()[["connections"]])
  } else {
    hint <- "Have you changed how the run is parallelized with `allow_par`?"
    vctrs::vec_cast(allow_par, integer(), call = call)
  }
  nlive_threaded <- thread_nlive(nlive, workers, hint, call)
  print(nlive_threaded)
  thread_envs <- split_live(x$live_env, nlive_threaded, sampler_info, call)
  m <- mirai::mirai_map(
    thread_envs,
    \(thread) {
      live_env <- list2env(thread$env, parent = emptyenv())
      res <- fn(
        live_env,
        lrps = l,
        thread$info,
        control = rc,
        show_progress = FALSE
      )
      list(
        "live" = rlang::env_get_list(
          live_env,
          c("unit", "log_lik", "birth_lik")
        ),
        "dead" = res
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
  result <- collect_results(results, thread_envs)
  env_bind(x$live_env, !!!result$live)
  new_ernest_run(x, result$dead)
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

  lapply(splits, \(spl) {
    list(
      "env" = list(
        "unit" = env_get(live_env, "unit")[spl, , drop = FALSE],
        "log_lik" = env_get(live_env, "log_lik")[spl],
        "birth_lik" = env_get(live_env, "birth_lik")[spl]
      ),
      "info" = split_info(spl, info),
      "split" = spl
    )
  })
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

collect_results <- function(results, threads) {
  runs <- .mapply(
    \(r, t) {
      parsed <- parse_results(r$dead, r$live)
      df <- vctrs::vec_rbind(
        vctrs::data_frame(!!!parsed$dead),
        vctrs::data_frame(!!!parsed$live)
      )
      df$id <- t$split[df$id]
      df
    },
    list(r = results, t = threads),
    MoreArgs = NULL
  )
  all_runs <- vctrs::vec_rbind(!!!runs)
  merged <- merge_results(all_runs)
  merged
}
