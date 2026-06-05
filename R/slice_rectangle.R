#' Slice sampling within a bounding hyperrectangle
#'
#' Propose new live points by evolving an existing live point using slice
#' sampling inside a bounding hyperrectangle. The rectangle is shrunk when a
#' proposal is rejected.
#'
#' @param steps `[integer(1)]` Number of reslicing iterations per proposal.
#'
#' @returns `[slice_rectangle]` A list inheriting from `ernest_lrps`.
#'
#' @details
#' Sampling starts from a live point that meets the likelihood criterion. Each
#' iteration draws a uniform proposal inside the rectangle and accepts it if it
#' satisfies the criterion. Rejected proposals cause the rectangle to be
#' shrunk so the rejected point lies on the boundary and the original point
#' remains inside. The process repeats until a valid proposal is found or the
#' rectangle can no longer be reduced.
#'
#' Use `steps` to increase how many times a point goes through the slicing
#' procedure before being returned as a sample for the live set. One step
#' consists of one or more slicing operations, and each slice proceeds until
#' a point is found that satisfies the likelihood criterion.
#'
#' @inheritSection new_ernest_lrps Loop safety
#'
#' @references
#' Neal, R. M. (2000). Slice Sampling (Version 1). arXiv.
#' \doi{10.48550/ARXIV.PHYSICS/0009028}
#' Buchner, J. (2021). UltraNest - A Robust, General Purpose Bayesian Inference
#' Engine. Journal of Open Source Software, 6(60), 3001.
#' \doi{10.21105/joss.03001}
#'
#' @examples
#' lrps <- slice_rectangle()
#' adaptive_lrps <- slice_rectangle(max_steps = 30, adaptive = TRUE)
#'
#' @family ernest_lrps
#' @export
slice_rectangle <- function(
  steps = 3L
) {
  new_slice_rectangle(steps = steps)
}

#' @noRd
#' @export
format.slice_rectangle <- function(x, ...) {
  cli::format_inline(
    "Slice Sampling LRPS ({x$steps} Step{?s})"
  )
}

#' Create a new slice LRPS
#'
#' Internal constructor for the slice sampling LRPS.
#'
#' @param steps Number of times to resample from the parameter space.
#' @param ... Additional arguments passed to `new_ernest_lrps()`.
#'
#' @return An LRPS specification, a list with class
#' `c("slice_rectangle", "ernest_lrps")`.
#' @noRd
new_slice_rectangle <- function(
  steps = 3L,
  ...
) {
  check_number_whole(steps, min = 1)
  new_ernest_lrps(steps = as.integer(steps), ..., .class = "slice_rectangle")
}

#' @rdname propose
#' @export
propose.slice_rectangle <- function(
  x,
  original = NULL,
  criterion = -Inf
) {
  if (is.null(original)) {
    NextMethod(x)
  } else {
    res <- SliceImpl(
      original = original,
      unit_log_fn = x$unit_log_fn,
      criterion = criterion,
      steps = x$steps,
      max_loop = x$max_loop
    )
    env_poke(x$cache, "neval", x$cache$neval + res$neval)
    res
  }
}

#' @rdname update_lrps
#' @export
update_lrps.slice_rectangle <- function(x, unit = NULL, ...) {
  do.call(new_slice_rectangle, as.list(x))
}
