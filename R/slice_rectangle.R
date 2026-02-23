#' Generate new points with slice sampling
#'
#' Create new samples for the live set by evolving a current point in the set
#' through slice sampling within a bounding hyperrectangle, shrinking the
#' rectangle when proposals are rejected.
#'
#' @param enlarge `[double(1)]`\cr Factor by which to inflate the
#' hyperrectangle's volume before sampling (see Details). Optional, and must be
#' greater or equal to 1 if provided; if left `NA`, sampling is initially
#' bounded by the unit hypercube at each iteration.
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
#' By default, the hyperrectangle spans the extreme values of the current
#' live set in each dimension. This may risk excluding valid regions
#' of the parameter space, particularly where the posterior is multimodal or
#' highly non-Gaussian. To mitigate this, set `enlarge > 1`, which inflates the
#' hyperrectagle's volume by the specified factor before sampling. Setting
#' `enlarge` to `NA` disables this behaviour, instead slicing from the unit
#' hypercube at each iteration.
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
#' patient_lrps <- slice_rectangle(enlarge = 1.25)
#'
#' @family ernest_lrps
#' @export
slice_rectangle <- function(enlarge = 1) {
  check_number_decimal(enlarge, min = 1, allow_na = TRUE)
  new_slice_rectangle(enlarge = enlarge)
}

#' @noRd
#' @export
format.slice_rectangle <- function(x, ...) {
  enlarge_str <- if (is.na(x$enlarge)) {
    NULL
  } else {
    cli::format_inline(
      "(enlarged by {x$enlarge})"
    )
  }
  cli::format_inline(
    "Slice Sampling LRPS {enlarge_str}"
  )
}

#' Create a new slice LRPS
#'
#' Internal constructor for the slice sampling LRPS.
#'
#' @param unit_log_fn Function for computing log-likelihood in unit space.
#' @param n_dim  Number of dimensions.
#' @param max_loop  Maximum number of proposal attempts.
#' @param enlarge Inflate the hyperrectangle's volume before sampling.
#' @param cache Optional cache environment.
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
  enlarge = 1.25,
  cache = NULL
) {
  check_number_decimal(enlarge, min = 1, allow_na = TRUE)
  cache <- cache %||% new_environment()
  env_poke(cache, "n_accept", 0L)
  if (!is.null(n_dim) && n_dim >= 1L) {
    env_cache(cache, "lower", rep(0.0, n_dim))
    env_cache(cache, "upper", rep(1.0, n_dim))
  }
  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    enlarge = as.double(enlarge),
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
      lower = env_get(x$cache, "lower", rep(0.0, x$n_dim)),
      upper = env_get(x$cache, "upper", rep(1.0, x$n_dim)),
      max_loop = x$max_loop
    )
    env_poke(x$cache, "neval", x$cache$neval + res$neval)
    res
  }
}

#' @rdname update_lrps
#' @export
update_lrps.slice_rectangle <- function(x, unit = NULL, ...) {
  if (is.null(unit) || is.na(x$enlarge)) {
    return(do.call(new_slice_rectangle, as.list(x)))
  }
  lower <- matrixStats::colMins(unit)
  upper <- matrixStats::colMaxs(unit)
  if (x$enlarge > 1) {
    center <- (lower + upper) / 2
    radius <- (upper - lower) / 2
    new_radius <- radius * x$enlarge^(1 / x$n_dim)
    lower <- pmax(center - new_radius, 0.0)
    upper <- pmin(center + new_radius, 1.0)
  }
  env_bind(x$cache, lower = lower, upper = upper)
  do.call(new_slice_rectangle, as.list(x))
}
