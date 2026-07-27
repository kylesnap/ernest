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
  if (isTRUE(all.equal(x$rcrd, y$rcrd))) {
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
  c(rcrd, nlive) %<-% merge_rcrds(!!!elems, sep = "")

  # Update the sampler
  x$nlive <- nlive
  new_ernest_run(x, rcrd, .merge = glance)
}

#' Merge a list of `ernest_rcrd` objects together.
#'
#' @param ... ernest rcrd objects to merge together.
#' @param sep A character string to separate the ID of each run in `...` from
#' its name. Leave `NULL` if no renaming should be performed.
#'
#' @returns A list with two elements: `rcrd`, the merged `ernest_rcrd` object,
#' and `nlive`, the number of live points in the merged run.
#' @noRd
merge_rcrds <- function(
  ...,
  sep = NULL,
  invalid_run = "error",
  call = caller_env()
) {
  # Rename IDs if requested
  check_character(sep, allow_null = TRUE)
  elems <- dots_list(..., .named = !is.null(sep), .homonyms = "error")
  invalid_run <- arg_match(invalid_run)
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
  out <- unchop_rcrds(elems, nlive = nlive)
  list("rcrd" = out, "nlive" = nlive)
}

#' Combine a list of `ernest_rcrd` objects into a single object.
#'
#' @param rcrds A list of `ernest_rcrd` objects to combine.
#' @param nlive The total number of live points in the combined object.
#' @param call The calling environment.
#' @param arg The argument name for the `rcrds` parameter.
#' @returns A single `ernest_rcrd` object containing the combined records.
#' @noRd
unchop_rcrds <- function(rcrds, nlive) {
  rcrds <- lapply(rcrds, vctrs::vec_sort)
  niters <- vctrs::list_sizes(rcrds)
  nout <- sum(niters)
  merge_ord <- order(vec_c(!!!rcrds, .ptype = rcrds[[1]]))
  inv_ord <- vctrs::vec_chop(order(merge_ord), sizes = niters)

  # Get the number of live points in the merged records
  nlives <- matrix(NA_integer_, nrow = nout, ncol = length(rcrds))
  for (i in seq_along(rcrds)) {
    nlives[inv_ord[[i]], i] <- vctrs::field(rcrds[[i]], "nlive")
  }
  nlives <- vctrs::vec_fill_missing(nlives, "up")

  # Merge the records and update the number of live points
  nlives <- .rowSums(nlives, nout, length(rcrds), na.rm = TRUE)
  merged_rcrd <- vec_c(!!!rcrds)[merge_ord]
  vctrs::field(merged_rcrd, "nlive") <- as.integer(nlives)
  check_rcrd(merged_rcrd, nlive = nlive)
  merged_rcrd
}
