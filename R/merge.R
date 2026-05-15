#' Merge `ernest_run` objects together.
#'
#' Combines the samples from a set of nested sampling runs with differing
#' numbers of live points into a single run.
#'
#' @param x [[ernest_run]]\cr An object containing a nested sampling run.
#' @param y [[ernest_run]]\cr Another nested sampling run to merge with `x`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Additional `ernest_run`
#' objects to merge with `x`. Ignored if `y` is used.
#' @param .keep [[character(1)]] Strategy for merging the live set of the runs.
#' * `"first"`: The live set begins after the worst live point appears in the
#' run. This is the default, and is the most straightforward way to merge runs
#' with different numbers of live points.
#' * `"all"`: The live set gets smaller as points die throughout the run. This
#' is more complicated, but can preserve more information about the live set
#' if one run ends far earlier than another.
#'
#' @returns [[ernest_run]] An object containing the merged nested sampling run.
#' The `id` field of the [[ernest_rcrd]] component is updated to ensure IDs
#' range from `1` to the total number of points across all runs.
#'
#' @export
merge.ernest_run <- function(x, y = NULL, ..., .keep = c("first", "all")) {
  y_arg <- if (!is.null(y)) caller_arg(y) else "`...`"
  y <- if (is.null(y)) {
    list2(...)
  } else {
    c(list(y), list2(...))
  }

  # Check that all runs have the expected type and shape
  for (yi in y) {
    check_class(yi, "ernest_run", arg = y_arg)
    if (attr(yi$rcrd, "nvar") != attr(x$rcrd, "nvar")) {
      cli::cli_abort(
        "`{y_arg}` must have the same number of variables as `{caller_arg(x)}`."
      )
    }
  }

  # Merge objects together
  merged_rcrd <- x$rcrd
  nlive <- 0L
  for (yi in y) {
    m <- merge_rcrd(merged_rcrd, yi$rcrd, keep = .keep)
    nlive <- attr(m, "nlive")
    merged_rcrd <- m
  }

  # Update the sampler
  old_nlive <- x$nlive
  x$first_update <- as.integer((x$first_update / old_nlive) * nlive)
  x$update_interval <- as.integer((x$update_interval / old_nlive) * nlive)
  x$nlive <- nlive

  new_ernest_run(x, merged_rcrd)
}

merge_rcrd <- function(
  x,
  y,
  keep = c("first", "all"),
  invalid_run = c("error", "warn", "quiet")
) {
  keep <- arg_match(keep)
  invalid_run <- arg_match(invalid_run)
  # Reindex the IDs of each group
  x_ids <- vctrs::vec_group_id(field(x, "id"))
  y_ids <- vctrs::vec_group_id(field(y, "id"))
  nlive <- attr(x_ids, "n") + attr(y_ids, "n")
  y_ids <- y_ids + attr(x_ids, "n")
  attributes(x_ids) <- NULL
  attributes(y_ids) <- NULL
  vctrs::field(x, "id") <- x_ids
  vctrs::field(y, "id") <- y_ids

  # Merge the points together and sort them
  out <- sort(c(x, y))
  out <- if (keep == "first") {
    merge_rcrd_first(out, nlive)
  } else {
    merge_rcrd_all(out, nlive)
  }
  try_fetch(
    rcrd_is_run(out, nlive = nlive),
    warn = \(cnd) {
      switch(
        invalid_run,
        "warn" = cli::cli_warn("`merge` produced an invalid run.", cnd),
        "error" = cli::cli_abort("`merge` failed.", cnd),
        "quiet" = NULL
      )
    }
  )
  structure(
    vctrs::vec_cast(out, new_ernest_rcrd()),
    "nlive" = nlive
  )
}

#' Live set defined by appearance of first dead point.
#' @noRd
merge_rcrd_first <- function(x, nlive) {
  id_loc <- vctrs::vec_group_loc(field(x, "id"))
  first_live <- min(vapply(id_loc$loc, \(x) x[[length(x)]], integer(1)))

  dead_pts <- vec_c(
    !!!lapply(id_loc$loc, \(idx) idx[idx < first_live]),
    ptype = integer()
  )
  live_pts <- vapply(id_loc$loc, \(idx) idx[idx >= first_live][[1]], integer(1))

  # Reassign `0` neval to the live set
  vctrs::field(x[live_pts], "neval") <- rep_along(live_pts, 0L)

  # Reassign nlive
  x <- x[sort(c(dead_pts, live_pts))]
  vctrs::field(x, "nlive") <- get_points(
    field(x, "log_lik"),
    nlive,
    add_live = TRUE
  )
  x
}

#' Live set gets smaller as points die throughout the run.
#' @noRd
merge_rcrd_all <- function(x, nlive) {
  id_loc <- vctrs::vec_group_loc(field(x, "id"))
  # Remove `nlive` at the iteration after each point's death
  death_idx <- vapply(id_loc$loc, \(idx) idx[[length(idx)]], integer(1)) + 1
  death_idx <- vctrs::num_as_location(death_idx, length(x), oob = "remove")
  death_nlive <- integer(length(x))
  death_nlive[death_idx] <- -1L
  death_nlive <- cumsum(death_nlive)

  # Reassign `0` neval to the live set
  vctrs::field(x[death_idx - 1], "neval") <- rep_along(death_idx, 0L)

  # Reassign nlive
  death_nlive <- death_nlive +
    get_points(field(x, "log_lik"), nlive, add_live = FALSE)
  vctrs::field(x, "nlive") <- death_nlive
  x
}
