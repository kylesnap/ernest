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
  rcrd <- nlive <- NULL
  c(rcrd, nlive) %<-% merge_rcrd(x$rcrd, y$rcrd, suffix = suffix)

  # Update the sampler
  old_nlive <- x$nlive
  x$first_update <- as.integer((x$first_update / old_nlive) * nlive)
  x$update_interval <- as.integer((x$update_interval / old_nlive) * nlive)
  x$nlive <- nlive
  new_ernest_run(x, rcrd, .merge = glance)
}

#' Merge two `ernest_rcrd` objects together.
#'
#' @param x,y ernest rcrd objects to merge together.
#' @param suffix Suffixes to append to the IDs of `x` and `y`
#' if there are any duplicate IDs.
#' @param invalid_run Action to take if the merged rcrd fails validation with
#' `check_rcrd()`. One of `"error"`, `"warn", or `"quiet"`.
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
  out <- compile_rcrd(sort(vec_c(x, y)), nlive)
  tryCatch(
    check_rcrd(out, nlive = nlive, sorted = TRUE),
    ernest_bad_run_rcrd = function(cnd) {
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
