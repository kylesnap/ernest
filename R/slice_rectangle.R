#' Generate new points with slice sampling
#'
#' Create new samples for the live set by evolving a current point in the set
#' through slice sampling within a bounding hyperrectangle, shrinking the
#' rectangle when proposals are rejected.
#'
#' @param steps `[integer(1)]`\cr Number of times to reslice points during
#' sampling. Must be larger than zero. Higher values increase the number of
#' reslicing steps, improving exploration of complex or multimodal posteriors.
#'
#' @returns `[slice_rectangle]`, a named list that inherits from
#' [[ernest_lrps]].
#'
#' @details
#' The slice LRPS generates proposals by uniformly sampling within a bounding
#' hyperrectangle that contains regions of the parameter space satisfying the
#' likelihood criterion. Sampling begins by selecting a known live point
#' \eqn{\theta} that satisfies the criterion. Each iteration proposes
#' a new point within this rectangle via uniform sampling and compares it
#' against the criterion; if rejected, a new hyperrectangle is drawn such that
#' the proposed point is on its boundary and \eqn{\theta} is in its interior.
#' This continues until either a valid proposal is found or the rectangle has
#' shrunk to the point where no further clamping operations can be performed.
#'
#' To improve exploration, set `steps > 1`, which increases the number of
#' reslicing steps during sampling. This controls how many times the sampler
#' reslices points to better explore complex or multimodal posteriors.
#'
#' @references
#' Neal, R. M. (2000). Slice Sampling (Version 1). arXiv.
#' \doi{10.48550/ARXIV.PHYSICS/0009028}
#'
#' @examples
#' # Basic usage with default parameters
#' lrps <- slice_rectangle()
#'
#' # More patient sampler for complex posteriors
#' patient_lrps <- slice_rectangle(steps = 3)
#'
#' @family ernest_lrps
#' @export
slice_rectangle <- function(steps = 1) {
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
#' @param n_dim  Number of dimensions.
#' @param max_loop  Maximum number of proposal attempts.
#' @param steps Number of times to reslice points during sampling.
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
  n_dim = NULL,
  max_loop = 1e6L,
  steps = 1,
  cache = NULL,
  call = caller_env()
) {
  check_number_whole(steps, min = 1, call = call)
  cache <- cache %||% new_environment()
  env_poke(cache, "n_accept", 0L)
  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
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
update_lrps.slice_rectangle <- function(x, ...) {
  do.call(new_slice_rectangle, as.list(x))
}
