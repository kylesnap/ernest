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
#' If `first_update` or `update_interval` differ between runs, the defaults
#' from [[ernest_sampler]] are used instead.
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
  xy <- reindex_runs(as_ernest_rcrd(x), as_ernest_rcrd(y))
  results <- merge_results(!!!xy)
  z <- merge_sampler(x, y)
  # Reform sampler
  env_bind(
    z$live_env,
    !!!as.list(results$live)[c("unit", "log_lik", "birth_lik")]
  )
  z <- refresh_ernest_sampler(z)
  new_ernest_run_(z, results$dead)
}

#' Reindex runs so they have continuous IDs from 1 to 'nlive'
#'
#' @param ... ernest_rcrd objects.
#'
#' @return A list of ernest_rcrd objects with reindexed IDs, same length as
#' the dots.
#' @noRd
reindex_runs <- function(...) {
  runs <- list2(...)
  nlives <- vapply(
    runs,
    \(x) vctrs::vec_unique_count(field(x, "id")),
    integer(1)
  )
  .mapply(
    \(x, start) {
      vctrs::field(x, "id") <- as.integer(
        vctrs::vec_group_id(field(x, "id")) + start
      )
      x
    },
    dots = list(runs, c(0, cumsum(nlives)[-length(nlives)])),
    MoreArgs = NULL
  )
}

#' Merge the dead points from multiple nested sampling runs.
#'
#' @param ... A set of ernest_rcrd. Each rcrd should have unique
#' IDs, else an error is thrown.
#'
#' @importFrom vctrs `field<-`
#' @returns Two lists of "dead" and "live" run information.
#' @noRd
merge_results <- function(..., .call = caller_env()) {
  runs <- list2(...)
  ids <- vctrs::vec_c(
    !!!lapply(
      runs,
      \(x) vctrs::vec_unique(field(x, "id"))
    ),
    .error_call = call
  )
  if (vctrs::vec_duplicate_any(ids)) {
    cli::cli_abort("All runs in `...` must contain unique IDs.", call = .call)
  }
  all_runs <- sort(vctrs::vec_c(!!!runs))

  # Get live points
  first_live <- match(TRUE, field(all_runs, "evals") == 0L)
  min_live <- field(all_runs, "log_lik")[[first_live]]
  live <- vctrs::vec_c(
    !!!lapply(
      vctrs::vec_split(all_runs, field(all_runs, "id"))$val,
      \(id_rows) id_rows[match(TRUE, field(id_rows, "log_lik") >= min_live)]
    ),
    .error_call = call
  )
  field(live, "evals") <- rep(0L, length(live))
  # Get dead points
  dead <- vctrs::vec_slice(
    all_runs,
    i = seq_len(first_live - 1L),
    error_call = .call
  )
  list(
    "live" = live[order(field(live, "id"))],
    "dead" = dead,
    "ndrop" = length(all_runs) - (length(live) + length(dead))
  )
}

#' Combine two `ernest_sampler` objects together.
#'
#' @param x,y `ernest_sampler` objects.
#'
#' @returns A single `ernest_sampler` object.
#' @noRd
merge_sampler <- function(x, y) {
  nlive <- x$nlive + y$nlive
  first_update <- if (x$first_update != y$first_update) {
    as.integer(nlive * 2.5)
  } else {
    x$first_update
  }
  update_interval <- if (x$update_interval != y$update_interval) {
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
