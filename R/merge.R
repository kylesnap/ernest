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
  elems <- list(x$rcrd, y$rcrd)
  names(elems) <- suffix
  c(rcrd, nlive) %<-% merge_rcrd(!!!elems, sep = "")

  # Update the sampler
  old_nlive <- x$nlive
  x$first_update <- as.integer((x$first_update / old_nlive) * nlive)
  x$update_interval <- as.integer((x$update_interval / old_nlive) * nlive)
  x$nlive <- nlive
  new_ernest_run(x, rcrd, .merge = glance)
}

#' Merge two `ernest_rcrd` objects together.
#'
#' @param ... ernest rcrd objects to merge together.
#' @param sep A character string to separate the ID of each run in `...` from
#' its name. Leave `NULL` if no renaming should be performed.
#' @param invalid_run Action to take if the merged rcrd fails validation with
#' `check_rcrd()`. One of `"error"`, `"warn", or `"quiet"`.
#'
#' @returns A list with two elements: `rcrd`, the merged `ernest_rcrd` object,
#' and `nlive`, the number of live points in the merged run.
#' @noRd
merge_rcrd <- function(
  ...,
  sep = NULL,
  invalid_run = c("error", "warn", "quiet")
) {
  check_character(sep, allow_null = TRUE)
  elems <- dots_list(..., .named = !is.null(sep), .homonyms = "error")
  invalid_run <- arg_match(invalid_run)

  # Relabel the IDs of each group
  if (!is.null(sep)) {
    elems <- .mapply(
      \(x, nm) {
        vctrs::field(x, "id") <- paste(field(x, "id"), nm, sep = sep)
        x
      },
      dots = list(elems, names(elems)),
      MoreArgs = NULL
    )
  }

  nlive <- sum(vapply(
    elems,
    function(x) vctrs::vec_unique_count(field(x, "id")),
    integer(1L)
  ))
  out <- Reduce(merge_rcrd_exact, elems)
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

merge_rcrd_exact <- function(x_rcrd, y_rcrd) {
  x_rcrd <- sort(x_rcrd)
  y_rcrd <- sort(y_rcrd)

  nx <- length(x_rcrd)
  ny <- length(y_rcrd)
  nout <- nx + ny

  x_idx <- integer(nx)
  y_idx <- integer(ny)
  nlive <- integer(nout)

  ix <- iy <- io <- 1L
  while (ix <= nx || iy <= ny) {
    x_nlive <- if (ix <= nx) field(x_rcrd[[ix]], "nlive") else 0L
    y_nlive <- if (iy <= ny) field(y_rcrd[[iy]], "nlive") else 0L
    cur_nlive <- x_nlive + y_nlive
    nlive[[io]] <- cur_nlive

    x_loglik <- if (ix <= nx) field(x_rcrd[[ix]], "log_lik") else -Inf
    y_loglik <- if (iy <= ny) field(y_rcrd[[iy]], "log_lik") else -Inf
    take_x <- iy > ny || (ix <= nx && x_loglik <= y_loglik)

    if (take_x) {
      x_idx[[ix]] <- io
      ix <- ix + 1L
    } else {
      y_idx[[iy]] <- io
      iy <- iy + 1L
    }
    io <- io + 1L
  }

  merged_rcrd <- vctrs::list_combine(
    x = list(x_rcrd, y_rcrd),
    indices = list(x_idx, y_idx),
    size = nout
  )
  vctrs::field(merged_rcrd, "nlive") <- nlive
  merged_rcrd
}
