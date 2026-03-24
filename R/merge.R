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
  z <- merge_sampler(x, y)
  # Merge results
  list_x <- butcher(x)
  list_y <- butcher(y, first_id = max(list_c(list_x$dead_id)) + 1L)
  results <- merge_results(list_x, list_y)
  # Reform sampler
  env_bind(z$live_env, !!!results$live)
  z <- refresh_ernest_sampler(z)
  new_ernest_run(z, results$dead)
}

#' Merge the dead points from multiple nested sampling runs.
#'
#' @param ... Lists of nested sampling results, each containing "unit",
#' "log_lik", "id", "evals", and "birth_lik" elements.
#' @param .call Information about the calling environment for error messages.
#' @returns A list containing merged "live" and "dead" points from the input
#' runs.
#'
#' @noRd
merge_results <- function(..., .call = caller_env()) {
  runs <- list2(...)
  flattened <- lapply(runs, \(r) parse_results(r))
  results <- list(
    "unit" = do.call(rbind, lapply(flattened, `[[`, "unit")),
    "log_lik" = do.call(c, lapply(flattened, `[[`, "log_lik")),
    "id" = do.call(c, lapply(flattened, `[[`, "id")),
    "evals" = do.call(c, lapply(flattened, `[[`, "evals")),
    "birth_lik" = do.call(c, lapply(flattened, `[[`, "birth_lik"))
  )
  order <- order(results$log_lik)
  results <- list(
    "unit" = results$unit[order, , drop = FALSE],
    "log_lik" = results$log_lik[order],
    "id" = results$id[order],
    "evals" = results$evals[order],
    "birth_lik" = results$birth_lik[order]
  )
  # Get live points
  first_live <- match(TRUE, results$evals == 0L)
  min_live <- results$log_lik[[first_live]]
  ordered <- data.frame(
    log_lik = results$log_lik,
    id = results$id,
    .iter = seq_along(results$log_lik)
  )
  live_idx <- vapply(
    split(ordered, ordered$id),
    \(df) df$.iter[match(TRUE, df$log_lik >= min_live)],
    integer(1)
  )
  live <- list(
    unit = results$unit[live_idx, , drop = FALSE],
    log_lik = results$log_lik[live_idx],
    birth_lik = results$birth_lik[live_idx]
  )
  # Get dead points
  dead_idx <- seq_len(first_live - 1L)
  dead <- list(
    "dead_unit" = asplit(results$unit[dead_idx, , drop = FALSE], 1),
    "dead_log_lik" = vctrs::list_of(
      results$log_lik[dead_idx],
      .ptype = double()
    ),
    "dead_id" = vctrs::list_of(results$id[dead_idx], .ptype = integer()),
    "dead_evals" = vctrs::list_of(results$evals[dead_idx], .ptype = integer()),
    "dead_birth" = vctrs::list_of(
      results$birth_lik[dead_idx],
      .ptype = double()
    )
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

#' Simplify nested sampling results into a basic list for merging, similar
#' to the output of the nested_sampling_impl function.
#'
#' @param x An ernest_run object.
#' @param first_id The ID of the first point in the nested sampling run.
#'
#' @returns A list containing a subset of the elements from `x`.
#' @noRd
butcher <- function(x, first_id = 1, call = caller_env()) {
  check_number_whole(first_id, min = 1, call = call)
  check_class(x, "ernest_run", call = call)
  nlive <- length(unique(x$weights$id))
  ids <- data.frame(
    "new" = seq(from = first_id, length.out = nlive),
    "old" = unique(x$weights$id)
  )
  dead_id <- ids$new[match(x$weights$id, ids$old)]
  list(
    "dead_log_lik" = vctrs::list_of(x$weights$log_lik, .ptype = double()),
    "dead_id" = vctrs::list_of(dead_id, .ptype = integer()),
    "dead_evals" = vctrs::list_of(x$weights$evaluations, .ptype = integer()),
    "dead_birth" = vctrs::list_of(x$weights$birth_lik, .ptype = double()),
    "dead_unit" = asplit(x$samples$unit_cube, 1)
  )
}
