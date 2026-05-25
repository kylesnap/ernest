#' Merge `ernest_run` objects together.
#'
#' Combines the samples from a set of nested sampling runs with differing
#' numbers of live points into a single run.
#'
#' @param x [[ernest_run]]\cr An object containing a nested sampling run.
#' @param y [[ernest_run]]\cr Another nested sampling run to merge with `x`.
#' @param suffix `[[character(2)]]` Suffixes to append to the IDs of `x` and `y`
#' if there are any duplicate IDs.
#' @param keep `[[character(1)]]` Specifies what live points to retain from
#' merging runs together:
#' * `"first"`: The live set begins after the worst live point appears in the
#' run. This is the default, and is the most straightforward way to merge runs
#' with different numbers of live points.
#' * `"all"`: The live set gets smaller as points die throughout the run. This
#' is more complicated, but can preserve more information about the live set
#' if one run ends far earlier than another.
#' @inheritParams rlang::check_dots_empty
#'
#' @returns [[ernest_run]] An object containing the merged nested sampling run.
#'
#' @export
merge.ernest_run <- function(
  x,
  y,
  suffix = c(".x", ".y"),
  keep = c("first", "all"),
  ...
) {
  check_class(y, "ernest_run")
  check_dots_empty()

  # Merge objects together
  merged_rcrd <- nlive <- NULL
  c(merged_rcrd, nlive) %<-% merge_rcrd(x$rcrd, y$rcrd, keep = keep)

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
  suffix = c(".x", ".y"),
  keep = c("first", "all"),
  invalid_run = c("error", "warn", "quiet")
) {
  suffix <- vec_cast(suffix, character(2))
  keep <- arg_match(keep)
  invalid_run <- arg_match(invalid_run)
  # Reindex the IDs of each group
  x_ids <- field(x, "id")
  y_ids <- field(y, "id")
  if (any(vctrs::vec_in(x_ids, y_ids))) {
    x_ids <- paste0(x_ids, if (!is.na(suffix[1])) suffix[1])
    y_ids <- paste0(y_ids, if (!is.na(suffix[2])) suffix[2])
    if (any(vctrs::vec_in(x_ids, y_ids))) {
      cli::cli_abort("IDs of `x` and `y` must be unique.")
    }
  }
  nlive <- vctrs::vec_unique_count(x_ids) + vctrs::vec_unique_count(y_ids)
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
  list("rcrd" = vctrs::vec_cast(out, ernest_rcrd()), "nlive" = nlive)
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
