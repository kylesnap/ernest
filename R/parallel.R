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
  parent_info,
  parent_control,
  show_progress,
  call = caller_env()
) {
  check_installed("mirai", "for parallel nested sampling.", call = call)
  mirai::require_daemons(call = call)
  nworkers <- mirai::info()[["connections"]]
  split_id <- thread_nlive(x, nworkers, call)
  split_x <- split_run(x, split_id, parent_control, parent_info)

  m <- mirai::mirai_map(
    split_x,
    \(sx) {
      library(ernest)
      live_env <- list2env(sx[c("unit", "log_lik", "birth_lik")])
      dead <- impl_(
        live_env,
        lrps_,
        sx$info,
        sx$control,
        show_progress = FALSE
      )
      list("dead" = dead, "live" = as.list(live_env))
    },
    impl_ = nested_sampling_impl,
    lrps_ = x$lrps
  )

  m_out <- mirai::collect_mirai(
    m,
    options = c(".stop", if (show_progress) ".progress" else NULL)
  )
  result <- reindex_threads(m_out, split_id)
  env_bind(
    x$live_env,
    !!!as.list(result$live)[c("unit", "log_lik", "birth_lik")]
  )
  new_ernest_run(x, result$dead)
}

#' Get the nlive for each worker.
#'
#' @param nlive The total nlive of the sampler.
#' @param workers An integer vector or NULL. The length of the vector is the
#' number of workers, the element is the nlive of each sub-sampler.
#' @param call Error info.
#'
#' @returns A validated integer vector, whose length is the number of
#' runs and each element is that run's nlive.
#' @noRd
thread_nlive <- function(x, nworkers, call = caller_env()) {
  nworkers <- min(x$nlive, nworkers)
  workers <- as.integer(pmax(1L, rep(x$nlive %/% nworkers, nworkers)))
  nlive_workers <- sum(workers)
  workers[[1]] <- workers[[1]] + (x$nlive - nlive_workers)
  preserve_seed(attr(x, "seed"))
  ids <- sample.int(x$nlive, size = x$nlive)
  vctrs::vec_chop(ids, sizes = workers)
}

#' Split the live set into a series of threads.
#'
#' @param x The sampler.
#' @param slices How the sampler is to be split.
#'
#' @returns Named list of IDs by worker and environments.
#' @noRd
split_run <- function(x, slices, parent_control, parent_info) {
  x_rcrd <- if (inherits_only(x, "ernest_sampler")) {
    NULL
  } else {
    as_ernest_rcrd(x)
  }

  lapply(slices, \(slice) {
    list(
      "unit" = env_get(x$live_env, "unit")[slice, , drop = FALSE],
      "log_lik" = env_get(x$live_env, "log_lik")[slice],
      "birth_lik" = env_get(x$live_env, "birth_lik")[slice],
      "info" = split_info(length(slice), parent_info),
      "control" = new_generate_control(
        parent_control$max_iterations,
        parent_control$max_evaluations,
        parent_control$min_logz,
        prev_run = if (!is.null(x_rcrd)) {
          vctrs::vec_slice(x_rcrd, field(x_rcrd, "id") %in% slice)
        },
        call = call
      )
    )
  })
}

split_info <- function(split_nlive, parent_info) {
  frac_nlive <- split_nlive / parent_info$nlive
  list(
    seed = parent_info$seed,
    nlive = split_nlive,
    first_update = as.integer(parent_info$first_update * frac_nlive),
    update_interval = as.integer(parent_info$update_interval * frac_nlive)
  )
}

reindex_threads <- function(results, splits) {
  result <- mapply(
    \(res, split) {
      live <- extract_live_points(res$live, .id = split)
      vctrs::field(res$dead, "id") <- split[field(res$dead, "id")]
      vctrs::field(res$dead, "id") <- split[field(res$dead, "id")]
      vctrs::vec_c(res$dead, live)
    },
    res = results,
    split = splits
  )
  merge_results(vctrs::vec_c(!!!result))
}
