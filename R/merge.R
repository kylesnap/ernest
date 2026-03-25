#' Merge two nested sampling runs
#'
#' Combines two nested sampling runs into a single run by re-ordering their
#' samples by log-likelihood and reconstructing the live set.
#'
#' @param x,y [[ernest_run]]\cr Runs generated from compatible nested sampling
#' specifications.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns [[ernest_run]] A run containing the merged results from `x` and `y`.
#'
#' @details
#' The two runs must use the same prior variable names and LRPS method.
#' If `first_update` or `update_interval` differ between runs, a warning is
#' thrown and defaults are re-computed from the merged `nlive`.
#'
#' The merged run contains all dead points from both runs, ordered by
#' log-likelihood, and the live set is reconstructed from the remaining points.
#' This results in a single nested sampling run with `nlive` equal to the sum of
#' the two original runs.
#'
#' Note that `niter` is not simply the sum of the two original runs' `niter`
#' values; rather, the merged run's `niter` is equal to the total number of
#' samples drawn before either run's termination condition was met. This means
#' that the merged run's `niter` reflects the number of iterations it would take
#' to reach the same likelihood contour as the least advanced of the two
#' original runs.
#'
#' If the two runs do not share the same RNG seed, the merged result stores
#' `NA` as the seed.
#'
#' @examples
#' prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#'
#' run1 <- generate(
#'   ernest_sampler(ll_fn, prior, nlive = 100, seed = 24),
#'   max_iterations = 100
#' )
#' run2 <- generate(
#'   ernest_sampler(ll_fn, prior, nlive = 300, seed = 42),
#'   max_iterations = 100
#' )
#' merged <- merge(run1, run2)
#' summary(merged)
#' @export
merge.ernest_run <- function(x, y, ...) {
  check_class(y, "ernest_run")
  z <- merge_sampler(x, y)
  # Merge results
  list_x <- butcher_run(x, keep_live = TRUE)
  list_y <- butcher_run(y, keep_live = TRUE)
  merge_df <- merge_ids(list_x, list_y)
  results <- merge_results(merge_df)
  # Reform sampler
  env_bind(z$live_env, !!!results$live)
  z <- refresh_ernest_sampler(z)
  new_ernest_run(z, results$dead)
}

#' Correct the IDs of two merged runs.
#'
#' @noRd
merge_ids <- function(...) {
  runs <- list2(...)
  # Correct the IDS
  nlives <- vapply(runs, \(x) length(unique(x$id)), integer(1))
  runs <- .mapply(
    \(x, start) {
      x$id <- vctrs::vec_group_id(x$id) + start
      vctrs::data_frame(!!!x)
    },
    dots = list(runs, c(0, cumsum(nlives)[-length(nlives)])),
    MoreArgs = NULL
  )
  vctrs::vec_rbind(!!!runs)
}

#' Merge the dead points from multiple nested sampling runs.
#'
#' @param all_runs A dataframe of nested sampling results, merged together from
#' lists of butcher_run objects.
#'
#' @returns Two lists of "dead" and "live" run information.
#' @noRd
merge_results <- function(all_runs) {
  all_runs <- all_runs[order(all_runs$log_lik), ]
  # Get live points
  first_live <- match(TRUE, all_runs$evals == 0L)
  min_live <- all_runs$log_lik[[first_live]]
  live <- vctrs::vec_rbind(
    !!!lapply(
      vctrs::vec_split(all_runs, all_runs$id)$val,
      \(df) df[match(TRUE, df$log_lik >= min_live), ]
    )
  ) |>
    vctrs::df_list()
  live <- live[c("unit", "log_lik", "birth_lik")]
  # Get dead points
  dead <- vctrs::df_list(vctrs::vec_slice(all_runs, seq_len(first_live - 1L)))
  dead <- vctrs::df_list(
    "dead_unit" = dead$unit,
    "dead_log_lik" = dead$log_lik,
    "dead_id" = dead$id,
    "dead_evals" = dead$evals,
    "dead_birth" = dead$birth_lik
  )
  list("live" = live, "dead" = dead)
}

#' Combine two `ernest_sampler` objects together, warning the user if they are
#' not consistent.
#'
#' @param x,y `ernest_sampler` objects.
#' @param call Information about the calling environment for error messages.
#' @param ... Ignored.
#'
#' @returns A single `ernest_sampler` object.
#' @noRd
merge_sampler <- function(
  x,
  y,
  call = caller_env(),
  ...
) {
  check_class(x, "ernest_sampler", call = call)
  check_class(y, "ernest_sampler", call = call)
  x_arg <- caller_arg(x)
  y_arg <- caller_arg(y)
  if (!identical(x$prior$names, y$prior$names)) {
    cli::cli_abort(
      "`{x_arg}` and `{y_arg}` must have the same prior variable names.",
      call = call
    )
  }
  if (!identical(class(x$lrps), class(y$lrps))) {
    cli::cli_abort(
      "`{x_arg}` and `{y_arg}` must have the same LRPS method.",
      call = call
    )
  }
  nlive <- x$nlive + y$nlive
  first_update <- if (x$first_update != y$first_update) {
    cli::cli_warn(
      c(
        "`first_update` values differ between `{x_arg}` and `{y_arg}`",
        "!" = "Using default `nlive * 2.5`"
      ),
      call = call
    )
    as.integer(nlive * 2.5)
  } else {
    x$first_update
  }
  update_interval <- if (x$update_interval != y$update_interval) {
    cli::cli_warn(
      c(
        "`update_interval` values differ `{x_arg}` and `{y_arg}`",
        "!" = "Using default `nlive * 1.5`"
      ),
      call = call
    )
    as.integer(nlive * 1.5)
  } else {
    x$update_interval
  }
  seed <- if (!identical(attr(x, "seed"), attr(y, "seed"))) {
    NA_integer_
  } else {
    attr(x, "seed")
  }
  new_ernest_sampler(
    log_lik_fn = x$log_lik_fn,
    prior = x$prior,
    lrps = x$lrps,
    nlive = nlive,
    first_update = first_update,
    update_interval = update_interval,
    seed = seed
  )
}
