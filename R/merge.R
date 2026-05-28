#' Merge nested sampling runs together.
#'
#' Combines the samples from a set of nested sampling runs with differing
#' numbers of live points into a single run.
#'
#' @param x [[ernest_run]]\cr An object containing a nested sampling run.
#' @param y [[ernest_run]]\cr Another nested sampling run to merge with `x`.
#' @param suffix `[character(2)]`\cr Suffixes to append to the IDs of `x` and
#' `y` if there are any duplicate IDs.
#' @inheritParams rlang::check_dots_empty
#'
#' @returns [[ernest_run]] An object containing the merged nested sampling
#' results. An additional `.merge` is bound to the object, containing the
#' [[glance.ernest_run]] summary of both `x` and `y`.
#'
#' @details
#' Samples from two well-conditioned nested sampling runs can be merged together
#' to create a single run with a greater number of live points. This leads to a
#' more accurate estimate of the evidence and posterior distribution, as well as
#' a more precise estimate of the uncertainty on the evidence.
#'
#' @references Speagle, J. S. (2020). dynesty: A Dynamic Nested Sampling
#' Package for Estimating Bayesian Posteriors and Evidences. Monthly Notices of
#' the Royal Astronomical Society, 493, 3132–3158. \doi{10.1093/mnras/staa278}
#' @export
merge.ernest_run <- function(
  x,
  y,
  suffix = c(".x", ".y"),
  ...
) {
  check_class(y, "ernest_run")
  if (isTRUE(all.equal(x, y))) {
    cli::cli_abort(
      "`{caller_arg(x)}` and `{caller_arg(y)}` cannot be identical."
    )
  }
  check_dots_empty()

  # Get glances of the merged runs
  glance <- new_tibble0(vctrs::vec_rbind(glance(x), glance(y)))

  # Merge records together
  merged_rcrd <- nlive <- NULL
  c(merged_rcrd, nlive) %<-% merge_rcrd(x$rcrd, y$rcrd, suffix = suffix)

  # Update the sampler
  old_nlive <- x$nlive
  x$first_update <- as.integer((x$first_update / old_nlive) * nlive)
  x$update_interval <- as.integer((x$update_interval / old_nlive) * nlive)
  x$nlive <- nlive
  new_ernest_run(x, merged_rcrd, .merge = glance)
}

#' Merge two `ernest_rcrd` objects together.
#'
#' @param x,y ernest rcrd objects to merge together.
#' @param suffix Suffixes to append to the IDs of `x` and `y`
#' if there are any duplicate IDs.
#' @param invalid_run Action to take if the merged rcrd fails validation with
#' `rcrd_is_run()`. One of `"error"`, `"warn", or `"quiet"`.
#'
#' @returns A list with two elements: `rcrd`, the merged `ernest_rcrd` object,
#' and `nlive`, the number of live points in the merged run.
#' @noRd
merge_rcrd <- function(
  x,
  y,
  suffix = c(".x", ".y"),
  invalid_run = c("error", "warn", "quiet")
) {
  suffix <- vec_cast(suffix, character(2))
  invalid_run <- arg_match(invalid_run)
  # Reindex the IDs of each group
  x_ids <- field(x, "id")
  y_ids <- field(y, "id")
  if (any(vctrs::vec_in(x_ids, y_ids))) {
    x_ids <- paste0(x_ids, suffix[1])
    y_ids <- paste0(y_ids, suffix[2])
  }
  nlive <- vctrs::vec_unique_count(x_ids) + vctrs::vec_unique_count(y_ids)
  vctrs::field(x, "id") <- x_ids
  vctrs::field(y, "id") <- y_ids

  # Sort merged run and repair nlive.
  out <- compile_merged_rcrd(sort(vctrs::vec_c(x, y)), nlive)
  try_fetch(
    rcrd_is_run(out, nlive = nlive),
    warn = function(cnd) {
      switch(
        invalid_run,
        "warn" = cli::cli_warn(
          "`merge` produced an invalid run.",
          parent = cnd
        ),
        "error" = cli::cli_abort("`merge` failed.", parent = cnd),
        "quiet" = NULL
      )
    }
  )

  list("rcrd" = vctrs::vec_cast(out, ernest_rcrd()), "nlive" = nlive)
}

#' Repair `nlive` field of a merged run record
#'
#' @param rcrd A merged run record with an incorrect `nlive` field.
#' @param nlive The correct number of live points in the merged run.
#' @param unique_ids Whether to check that the merged rcrd contains `nlive`
#' unique IDs. Set to `FALSE` if the merged rcrd is known to contain duplicate
#' IDs (e.g., when merging resampled runs in `bootstraps()`).
#' @returns A repaired `ernest_rcrd` object with the correct `nlive` field.
#' @noRd
compile_merged_rcrd <- function(rcrd, nlive, unique_ids = TRUE) {
  if (is.unsorted(rcrd)) {
    cli::cli_abort("`{caller_arg(rcrd)}` must be sorted by `log_lik`.")
  }
  if (unique_ids && vctrs::vec_unique_count(field(rcrd, "id")) != nlive) {
    cli::cli_abort(c(
      "`{caller_arg(rcrd)}` must contain {nlive} unique IDs.",
      "x" = "Actually has {vctrs::vec_unique_count(field(rcrd, 'id'))}"
    ))
  }
  id_loc <- vctrs::vec_group_loc(field(rcrd, "id"))
  first_live_idx <- min(vapply(
    id_loc$loc,
    function(idx) idx[[length(idx)]],
    integer(1)
  ))

  # Sort run into DEAD and LIVE points
  dead_pts <- vctrs::vec_c(
    !!!lapply(id_loc$loc, function(idx) idx[idx < first_live_idx]),
    .ptype = integer()
  )
  live_pts <- vapply(
    id_loc$loc,
    function(idx) idx[idx >= first_live_idx][[1]],
    integer(1)
  )

  # Remerge and assign new NLIVE
  out <- rcrd[sort(c(dead_pts, live_pts))]
  vctrs::field(out, "nlive") <- get_points(
    field(out, "log_lik"),
    nlive,
    add_live = TRUE
  )
  out
}
