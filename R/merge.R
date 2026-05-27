#' Merge `ernest_run` objects together.
#'
#' Combines the samples from a set of nested sampling runs with differing
#' numbers of live points into a single run.
#'
#' @param x [[ernest_run]]\cr An object containing a nested sampling run.
#' @param y [[ernest_run]]\cr Another nested sampling run to merge with `x`.
#' @param suffix `[[character(2)]]` Suffixes to append to the IDs of `x` and `y`
#' if there are any duplicate IDs.
#' @inheritParams rlang::check_dots_empty
#'
#' @returns [[ernest_run]] An object containing the merged nested sampling run.
#'
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

  # Merge objects together
  merged <- merge_rcrd(x$rcrd, y$rcrd, suffix = suffix)
  merged_rcrd <- merged$rcrd
  nlive <- merged$nlive

  # Update the sampler
  old_nlive <- x$nlive
  x$first_update <- as.integer((x$first_update / old_nlive) * nlive)
  x$update_interval <- as.integer((x$update_interval / old_nlive) * nlive)
  x$nlive <- nlive
  new_ernest_run(x, merged_rcrd)
}

#' Merge two `ernest_rcrd` objects together.
#'
#' @params x,y ernest rcrd objects to merge together.
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
    x_ids <- paste0(x_ids, if (!is.na(suffix[1])) suffix[1])
    y_ids <- paste0(y_ids, if (!is.na(suffix[2])) suffix[2])
    if (any(vctrs::vec_in(x_ids, y_ids))) {
      cli::cli_abort("IDs of `x` and `y` must be unique.")
    }
  }
  nlive <- vctrs::vec_unique_count(x_ids) + vctrs::vec_unique_count(y_ids)
  vctrs::field(x, "id") <- x_ids
  vctrs::field(y, "id") <- y_ids

  out <- sort(vctrs::vec_c(x, y))
  id_loc <- vctrs::vec_group_loc(field(out, "id"))
  first_live <- min(vapply(
    id_loc$loc,
    function(idx) idx[[length(idx)]],
    integer(1)
  ))

  dead_pts <- vctrs::vec_c(
    !!!lapply(id_loc$loc, function(idx) idx[idx < first_live]),
    .ptype = integer()
  )
  live_pts <- vapply(
    id_loc$loc,
    function(idx) idx[idx >= first_live][[1]],
    integer(1)
  )

  neval <- field(out, "neval")
  neval[live_pts] <- 0L
  vctrs::field(out, "neval") <- neval

  out <- out[sort(c(dead_pts, live_pts))]
  vctrs::field(out, "nlive") <- get_points(
    field(out, "log_lik"),
    nlive,
    add_live = TRUE
  )

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
