#' Generate samples from multiple spanning ellipsoids
#'
#' Partitions the prior space into a set of ellipsoids whose union bounds
#' the set of live points. Samples are created by randomly
#' selecting an ellipsoid (weighted by their respective volumes), then using it
#' to generate a random point as in [unif_ellipsoid]. Effective for multimodal
#' posteriors where a single ellipsoid would be inefficient.
#'
#' @inheritParams unif_ellipsoid
#' @param min_reduction Double between 0 and 1. The minimum reduction in total
#' volume required for an ellipsoid to be split in two. Lower values lead to
#' more aggressive splitting.
#' @param allow_contact Logical. Whether to allow ellipsoids to overlap.
#'
#' @details
#' Nested likelihood contours for multimodal distributions are poorly
#' represented by a single ellipsoid. This method fits multiple ellipsoids to
#' better capture disconnected or elongated regions.
#'
#' Ellipsoids are generated using the following procedure:
#' 1. A single ellipsoid is fit to the set of live points, with volume \eqn{V}.
#' 2. The live points are clustered into two groups using k-means clustering.
#' 3. Ellipsoids are fit to each cluster.
#' 4. The split ellipsoids are accepted if all of the following conditions are
#' met:
#'     + Both ellipsoids are non-degenerate
#'     + The combined volume of the split ellipsoids is less than
#'     \eqn{min_{red.} * V}
#'     + (If `allow_contact` is `FALSE`) the ellipsoids do not intersect.
#' 5. Steps 2–4 are repeated recursively on each new ellipsoid until no further
#' splits are accepted, updating \eqn{V} to the volume of the currently split
#' ellipsoid.
#'
#' @inheritSection unif_ellipsoid Ellipsoids
#'
#' @returns A list with class `c("multi_ellipsoid", "ernest_lrps")`. Use with
#' [ernest_sampler()] to specify nested sampling behaviour.
#'
#' @references
#' Feroz, F., Hobson, M. P., Bridges, M. (2009) MULTINEST: An Efficient and
#' Robust Bayesian Inference Tool for Cosmology and Particle Physics. Monthly
#' Notices of the Royal Astronomical Society. 398(4), 1601–1614.
#' <https://doi.org/10.1111/j.1365-2966.2009.14548.x>
#'
#' @family ernest_lrps
#' @examples
#' data(example_run)
#' lrps <- multi_ellipsoid(enlarge = 1.25, min_reduction = 0.5)
#'
#' ernest_sampler(example_run$log_lik_fn, example_run$prior, sampler = lrps)
#' @export
multi_ellipsoid <- function(
  enlarge = 1.25,
  min_reduction = 0.7,
  allow_contact = TRUE
) {
  check_number_decimal(enlarge, min = 1)
  check_number_decimal(min_reduction, min = 0, max = 1)
  check_bool(allow_contact)

  if (enlarge == 1) {
    cli::cli_warn("`enlarge` is set to 1, which is not recommended.")
  }
  if (min_reduction == 1 && allow_contact) {
    cli::cli_warn(c(
      "`min_reduction` is set to 1, which may lead to over-splitting.",
      "i" = "Should `allow_contact` be set to `FALSE`?"
    ))
  }

  new_multi_ellipsoid(
    unit_log_fn = NULL,
    n_dim = NULL,
    enlarge = enlarge,
    min_reduction = min_reduction,
    allow_contact = allow_contact,
    max_loop = getOption("ernest.max_loop", 1e6L)
  )
}

#' @export
#' @noRd
format.multi_ellipsoid <- function(x, ...) {
  n_ell <- length(env_get(x$cache, "prob", double()))
  log_vol <- env_cache(x$cache, "total_log_volume", -Inf)
  cli::cli_format_method({
    cli::cli_text("No. Ellipsoids: {n_ell}")
    cli::cli_text("Total Log Volume: {pretty(log_vol)}")
    cli::cli_text("Min Reduction: {x$min_reduction}")
    cli::cli_text("Allow Contact: {x$allow_contact}")
    cli::cli_text("Enlargement: {x$enlarge}")
  })
}

#' Create a new multi_ellipsoid LRPS
#'
#' Internal constructor for multi ellipsoid LRPS objects.
#'
#' @param unit_log_fn Function to compute log-likelihood in unit space.
#' @param n_dim Integer. Number of dimensions.
#' @param max_loop Integer. Maximum proposal attempts.
#' @param cache Optional cache environment.
#' @param enlarge Double. Volume enlargement factor.
#' @param min_reduction Double. Minimum volume reduction for splitting.
#' @param allow_contact Logical. Whether ellipsoids can overlap.
#'
#' @return An LRPS specification, a list with class
#' `c("multi_ellipsoid", "ernest_lrps")`.
#' @noRd
new_multi_ellipsoid <- function(
  unit_log_fn = NULL,
  n_dim = NULL,
  max_loop = 1e6L,
  cache = NULL,
  enlarge = 1.0,
  min_reduction = 0.7,
  allow_contact = FALSE
) {
  check_number_decimal(enlarge, min = 1, allow_infinite = FALSE)
  check_number_decimal(min_reduction, min = 0, max = 1, allow_infinite = FALSE)
  check_bool(allow_contact)

  cache <- cache %||% new_environment()
  has_ell <- all(env_has(
    cache,
    c("prob", "ellipsoid", "total_log_volume")
  ))
  req_names <- c("center", "shape", "inv_sqrt_shape", "log_vol", "error")
  valid_ell <- has_ell && all(req_names %in% names(cache$ellipsoid[[1]]))
  if (!valid_ell && (is_integerish(n_dim) && n_dim > 0)) {
    ell <- MultiBoundingEllipsoids(
      matrix(double(), nrow = 0, ncol = n_dim),
      min_reduction = min_reduction,
      allow_contact = allow_contact
    )
    env_bind(
      cache,
      prob = ell$prob,
      ellipsoid = list(ell$ellipsoid),
      total_log_volume = ell$tot_log_vol
    )
  }

  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    enlarge = enlarge,
    min_reduction = min_reduction,
    allow_contact = allow_contact,
    .class = "multi_ellipsoid"
  )
}

#' @rdname propose
#' @export
propose.multi_ellipsoid <- function(
  x,
  original = NULL,
  criterion = -Inf
) {
  if (is.null(original)) {
    NextMethod(x)
  } else {
    ell_idx <- sample.int(length(x$cache$prob), size = 1, prob = x$cache$prob)
    ell <- x$cache$ellipsoid[[ell_idx]]
    res <- propose_ellipsoid(
      unit_log_fn = x$unit_log_fn,
      criterion = criterion,
      center = ell$center,
      inv_sqrt_shape = ell$inv_sqrt_shape,
      enlarge = x$enlarge,
      max_loop = x$max_loop
    )
    res <- c(res, ellipsoid_idx = ell_idx)
    env_poke(x$cache, "n_call", x$cache$n_call + res$n_call)
    res
  }
}

#' @rdname update_lrps
#' @export
update_lrps.multi_ellipsoid <- function(x, unit = NULL) {
  if (is.null(unit)) {
    return(do.call(new_multi_ellipsoid, as.list(x)))
  }
  splits <- MultiBoundingEllipsoids(
    unit,
    min_reduction = x$min_reduction,
    allow_contact = x$allow_contact
  )
  if (splits$ellipsoid[[1]]$error != 0L) {
    code <- splits$ellipsoid[[1]]$error
    cli::cli_warn(
      "Multi-ellipsoid fitting returned an error code ({code})."
    )
  }
  env_bind(
    x$cache,
    prob = splits$prob,
    ellipsoid = splits$ellipsoid,
    total_log_volume = splits$tot_log_vol
  )
  do.call(new_multi_ellipsoid, as.list(x))
}
