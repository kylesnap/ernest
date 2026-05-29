#' Slice sampling within a bounding hyperrectangle
#'
#' Propose new live points by evolving an existing live point using slice
#' sampling inside a bounding hyperrectangle. The rectangle is shrunk when a
#' proposal is rejected.
#'
#' @param steps `[integer(1)]` Number of reslicing iterations per proposal.
#' @param adaptive `[logical(1)]` If TRUE, adapt `steps` using the mean
#'   Mahalanobis distance among live points.
#' @param max_steps `[integer(1)]` Maximum `steps` allowed when
#'   `adaptive = TRUE`.
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
#' Use `steps > 1` to increase exploration. When `adaptive = TRUE`, `steps` is
#' increased or decreased depending on whether proposed moves are closer or
#' farther than the mean Mahalanobis distance of the live set.
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
  steps = 3L,
  adaptive = FALSE,
  max_steps = 100
) {
  new_slice_rectangle(steps = steps, adaptive = adaptive, max_steps = max_steps)
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
#' @param adaptive Whether or not to adapt the number of steps.
#' @param max_steps Maximum number of steps the sampler can reach when
#' `adaptive = TRUE`.
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
  adaptive = FALSE,
  max_steps = 100L,
  cache = NULL,
  call = caller_env()
) {
  check_number_whole(steps, min = 1, call = call)
  check_number_whole(max_steps, min = as.double(steps), call = call)
  check_bool(adaptive, call = call)
  cache <- cache %||% new_environment()

  if ((is_integerish(nvar) && nvar > 0)) {
    if (adaptive) {
      env_poke(cache, "distances", vctrs::list_of(.ptype = double()))
      env_cache(cache, "whitening", NA)
      env_cache(cache, "mean_dist", NaN)
    }
  }

  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    nvar = nvar,
    max_loop = max_loop,
    cache = cache,
    steps = as.integer(steps),
    adaptive = adaptive,
    max_steps = as.integer(max_steps),
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
    res <- if (x$adaptive) {
      sample <- SliceImplAdaptive(
        original = original,
        unit_log_fn = x$unit_log_fn,
        criterion = criterion,
        steps = x$steps,
        max_loop = x$max_loop,
        whitening = x$cache$whitening
      )
      x$cache$distances[[length(x$cache$distances) + 1L]] <- sample$distance
      sample
    } else {
      SliceImpl(
        original = original,
        unit_log_fn = x$unit_log_fn,
        criterion = criterion,
        steps = x$steps,
        max_loop = x$max_loop
      )
    }
    env_poke(x$cache, "neval", x$cache$neval + res$neval)
    res
  }
}

#' @rdname update_lrps
#' @export
update_lrps.slice_rectangle <- function(x, unit = NULL, ...) {
  if (!is.matrix(unit) || !x$adaptive) {
    return(do.call(new_slice_rectangle, as.list(x)))
  }

  if (is.finite(env_cache(x$cache, "mean_dist", NaN))) {
    if (length(x$cache$distances) < x$steps) {
      cli::cli_warn("Not enough history to adapt `steps`.")
      return(do.call(new_slice_rectangle, as.list(x)))
    }
    distances <- vctrs::vec_c(
      !!!env_get(x$cache, "distances"),
      .ptype = double()
    )
    moved_enough <- distances > env_get(x$cache, "mean_dist")

    new_steps <- x$steps
    for (mv in moved_enough) {
      if (mv) {
        new_steps <- min(new_steps - 1L, as.integer(new_steps / 1.1))
      } else {
        new_steps <- max(new_steps + 1L, as.integer(new_steps * 1.1))
      }
      new_steps <- max(1, min(x$max_steps, new_steps))
    }
    # print(sprintf("%d -> %d", x$steps, new_steps))
    x$steps <- as.integer(new_steps)
  }

  # Update covariance and mean distance
  precision <- solve(stats::cov(unit))
  mean_dist <- mean(stats::mahalanobis(
    unit,
    colMeans(unit),
    precision,
    inverted = TRUE
  ))
  # print(sprintf(
  #   "Mean Distance: %f -> %f",
  #   env_get(x$cache, "mean_dist"),
  #   mean_dist
  # ))
  env_poke(x$cache, "whitening", precision)
  env_poke(x$cache, "mean_dist", mean_dist)
  do.call(new_slice_rectangle, as.list(x))
}
