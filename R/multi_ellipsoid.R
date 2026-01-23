#' Generate new points from multiple spanning ellipsoids
#'
#' @description
#' Partitions the prior space into a set of ellipsoids whose union bounds
#' the live set. New points are created by randomly
#' selecting an ellipsoid (weighted by their respective volumes), then using it
#' to generate a random point as in [unif_ellipsoid]. Effective for multimodal
#' posteriors where a single ellipsoid would be inefficient.
#'
#' @inheritParams unif_ellipsoid
#'
#' @details
#' Nested likelihood contours for multimodal distributions are poorly
#' represented by a single ellipsoid. This method fits multiple ellipsoids to
#' better capture disconnected or elongated regions.
#'
#' Ellipsoids are generated using the following procedure:
#' 1. A single ellipsoid is fit to the live set, with volume \eqn{V}.
#' 2. The live set is clustered into two groups using k-means clustering.
#' 3. Ellipsoids are fit to each cluster.
#' 4. The split ellipsoids are accepted if both ellipsoids are non-degenerate,
#' and if the combined volume of the split ellipsoids is significantly
#' smaller than the original ellipsoid (calculated using Bayes' Information
#' Criterion).
#' 5. Steps 2–4 are repeated recursively on each new ellipsoid until no further
#' splits are accepted, updating \eqn{V} to the volume of the currently split
#' ellipsoid.
#'
#' @returns `[multi_ellipsoid]`, a named list that inherits from
#' [[ernest_lrps]].
#'
#' @references
#' * Feroz, F., Hobson, M. P., Bridges, M. (2009) MULTINEST: An Efficient and
#'   Robust Bayesian Inference Tool for Cosmology and Particle Physics. Monthly
#'   Notices of the Royal Astronomical Society. 398(4), 1601–1614.
#'   \doi{10.1111/j.1365-2966.2009.14548.x}
#' * Speagle, J. S. (2020). Dynesty: A Dynamic Nested Sampling Package for
#'   Estimating Bayesian Posteriors and Evidences. Monthly Notices of the
#'   Royal Astronomical Society, 493, 3132–3158.
#'   \doi{10.1093/mnras/staa278}
#'
#' @family ernest_lrps
#' @examples
#' data(example_run)
#' lrps <- multi_ellipsoid(enlarge = 1.25)
#'
#' ernest_sampler(example_run$log_lik_fn, example_run$prior, sampler = lrps)
#' @export
multi_ellipsoid <- function(
  enlarge = 1.25
) {
  check_number_decimal(enlarge, min = 1)

  if (enlarge == 1) {
    cli::cli_warn("`enlarge` is set to 1, which is not recommended.")
  }

  new_multi_ellipsoid(
    unit_log_fn = NULL,
    n_dim = NULL,
    enlarge = enlarge,
    max_loop = getOption("ernest.max_loop", 1e6L)
  )
}

#' @export
#' @noRd
format.multi_ellipsoid <- function(x, ...) {
  cli::format_inline(
    "Uniform sampling within bounding ellipsoids (enlarged by {x$enlarge})"
  )
}

#' Internal constructor for multi-ellipsoid LRPS objects
#'
#' @param unit_log_fn Computes log-likelihood in unit space.
#' @param n_dim Number of dimensions of the parameter space.
#' @param max_loop Maximum number of proposal attempts (default: 1e6).
#' @param cache Stores cached ellipsoid decompositions and related state.
#' @param enlarge Volume enlargement factor for each ellipsoid (must be ≥ 1).
#'
#' @returns An object of class `multi_ellipsoid`.
#'
#' @keywords internal
#' @noRd
new_multi_ellipsoid <- function(
  unit_log_fn = NULL,
  n_dim = NULL,
  max_loop = 1e6L,
  cache = NULL,
  enlarge = 1.0
) {
  check_number_decimal(enlarge, min = 1, allow_infinite = FALSE)

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
      point_log_volume = NA
    )
    env_bind(
      cache,
      prob = ell$prob,
      ellipsoid = ell$ellipsoid,
      total_log_volume = ell$tot_log_vol
    )
  }

  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    enlarge = enlarge,
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
    env_poke(x$cache, "neval", x$cache$neval + res$neval)
    res
  }
}

#' @rdname update_lrps
#' @export
update_lrps.multi_ellipsoid <- function(x, unit = NULL, log_volume = NA, ...) {
  if (is.null(unit)) {
    return(do.call(new_multi_ellipsoid, as.list(x)))
  }
  splits <- MultiBoundingEllipsoids(
    unit,
    point_log_volume = log_volume - log(nrow(unit))
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
