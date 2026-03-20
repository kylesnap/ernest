#' Merge two nested sampling runs
#'
#' Combines two nested sampling runs into a single run by re-ordering their
#' samples by log-likelihood and reconstructing the live set.
#'
#' @param x,y [[ernest_run]]\cr Runs generated from compatible nested sampling
#' specifications.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns [[ernest_run]] containing merged dead points and live points.
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
  list_y <- butcher(y, first_id = max(list_x$id) + 1)
  lik_unordered <- c(list_x$log_lik, list_y$log_lik)
  lik_ordered <- order(lik_unordered)
  xy_ordered <- data.frame(
    log_lik = lik_unordered[lik_ordered],
    id = c(list_x$id, list_y$id)[lik_ordered],
    evaluations = c(list_x$evaluations, list_x$evaluations)[lik_ordered],
    birth_lik = c(list_x$birth_lik, list_y$birth_lik)[lik_ordered],
    .iter = seq_along(c(list_x$birth_lik, list_y$birth_lik))
  )
  xy_unit <- rbind(list_x$unit, list_y$unit)[lik_ordered, , drop = FALSE]
  # Get dead points
  last_death <- match(TRUE, xy_ordered$evaluations == 0) - 1
  max_lik <- xy_ordered$log_lik[[last_death]]
  results <- {
    xy_dead <- xy_ordered[seq(last_death), ]
    list(
      "dead_unit" = xy_unit[seq(last_death), ],
      "dead_log_lik" = xy_dead$log_lik,
      "dead_id" = xy_dead$id,
      "dead_evals" = xy_dead$evaluations,
      "dead_birth" = xy_dead$birth_lik
    )
  }
  # Get live points
  live_idx <- vapply(
    split(xy_ordered, xy_ordered$id),
    \(df) df$.iter[match(TRUE, df$log_lik >= max_lik)],
    integer(1)
  )
  run_env <- new_environment(list(
    unit = xy_unit[live_idx, , drop = FALSE],
    log_lik = xy_ordered$log_lik[live_idx],
    birth_lik = xy_ordered$birth_lik[live_idx]
  ))
  # Reform sampler
  z$run_env <- run_env
  z <- refresh_ernest_sampler(z)
  new_ernest_run(z, results)
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
        "`first_update` values are different between `{x_arg}` and `{y_arg}`",
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
        "`update_interval` values are different between `{x_arg}` and `{y_arg}`",
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

#' Simplify nested sampling results into a basic list for merging.
#'
#' @param x An ernest_run object.
#' @param first_id The ID of the first point in the nested sampling run.
#'
#' @returns A list containing a subset of the elements from `x`.
#' @noRd
butcher <- function(x, first_id = 1, call = caller_env()) {
  check_number_whole(first_id, min = 1, call = call)
  check_class(x, "ernest_sampler", call = call)
  nlive <- length(unique(x$weights$id))
  id_seq <- seq(from = first_id, length.out = nlive)
  id_map <- setNames(as.integer(id_seq), unique(x$weights$id))
  list(
    log_lik = x$weights$log_lik,
    id = unname(id_map[as.character(x$weights$id)]),
    evaluations = x$weights$evaluations,
    birth_lik = x$weights$birth_lik,
    unit = x$samples$unit_cube
  )
}
