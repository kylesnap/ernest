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
#' @param unit_log_fn Function for computing log-likelihood in unit space.
#' @param nvar  Number of dimensions.
#' @param max_loop  Maximum number of proposal attempts.
#' @param steps Number of times to resample from the parameter space.
#' @param cache Optional cache environment.
#' @param call Error info.
#'
#' @srrstats {G2.4, G2.4a, G2.4b} Explicit conversion of inputs to expected
#' types or error messages for univariate inputs.
#'
#' @return An LRPS specification, a list with class
#' `c("slice_rectangle", "ernest_lrps")`.
#' @noRd
new_slice_rectangle <- function(
  unit_log_fn = NULL,
  nvar = NULL,
  max_loop = 1e6L,
  steps = 3L,
  cache = NULL,
  call = caller_env()
) {
  check_number_whole(steps, min = 1, call = call)
  cache <- cache %||% new_environment()

  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    nvar = nvar,
    max_loop = max_loop,
    cache = cache,
    steps = as.integer(steps),
    .class = "slice_rectangle"
  )
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
