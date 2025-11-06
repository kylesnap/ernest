#' Generate samples from the spanning ellipsoid
#'
#' Uses the bounding ellipsoid of the live points to define the region of prior
#' space that contains new points. Effective for unimodal and roughly-Gaussian
#' posteriors.
#'
#' @param enlarge Double, greater than or equal to 1. Factor by which to inflate
#' the bounding ellipsoid's volume before sampling (see Details).
#'
#' @returns A list with class `c("unif_ellipsoid", "ernest_lrps")`. Use with
#' [ernest_sampler()] to specify nested sampling behaviour.
#'
#' @details Nested likelihood contours rarely form perfect ellipses, so sampling
#' from the spanning ellipsoid without enlargement may exclude valid regions.
#' This can bias proposals towards the ellipsoid centre and overestimate
#' evidence. Setting `enlarge = 1` will produce a warning.
#'
#' The covariance matrix of the points is used to estimate the ellipsoid's
#' shape. In exceptional cases (e.g., perfect collinearity), this matrix may be
#' singular. Should this occur, the covariance matrix is reconditioned by
#' adjusting its eigenvalues. Should this also fail, the algorithm falls back
#' to sampling from the circumscribed sphere bounding the unit hypercube.
#'
#' @section Ellipsoids:
#' Ellipsoids are stored in the `cache` environment of the LRPS object.
#' Ellipsoids are defined by their centre \eqn{c} and shape matrix \eqn{A}.
#' The set of points \eqn{x} contained within the ellipsoid is given by
#' \deqn{ x \in {\bf{R}}^n | (x-c) A (x-c)' \leq 1 }
#'
#' The volume of the ellipsoid is
#' \eqn{V = \mathrm{Vol}(S_n) \sqrt{\det(A^{-1})}}, where
#' \eqn{\mathrm{Vol}(S_n)} is the volume of the unit hypersphere.
#'
#' For sampling, we store the matrix \eqn{A^{-1/2}}, the inverse of the
#' positive-semidefinite square root of \eqn{A}. The ellipsoid can equivalently
#' be defined as the set of points \deqn{x = A^{-1/2} y + c,}
#' where \eqn{y} are points from the unit hypersphere.
#'
#' For more on ellipsoids and their operations, see
#' [Algorithms for Ellipsoids](http://tcg.mae.cornell.edu/pubs/Pope_FDA_08.pdf)
#' by S.B. Pope, Cornell University Report FDA 08-01 (2008).
#'
#' @references
#' Feroz, F., Hobson, M. P., Bridges, M. (2009) MULTINEST: An Efficient and
#' Robust Bayesian Inference Tool for Cosmology and Particle Physics. Monthly
#' Notices of the Royal Astronomical Society. 398(4), 1601–1614.
#' \doi{10.1111/j.1365-2966.2009.14548.x}
#'
#' Mukherjee, P., Parkinson, D., & Liddle, A. R. (2006). A Nested Sampling
#' Algorithm for Cosmological Model Selection. The Astrophysical Journal,
#' 638(2), L51. \doi{10.1086/501068}
#'
#' @family ernest_lrps
#' @examples
#' data(example_run)
#' lrps <- unif_ellipsoid(enlarge = 1.25)
#'
#' ernest_sampler(example_run$log_lik_fn, example_run$prior, sampler = lrps)
#' @export
unif_ellipsoid <- function(enlarge = 1.25) {
  check_number_decimal(enlarge, min = 1)
  if (enlarge == 1.0) {
    cli::cli_alert_warning("`enlarge` is set to 1.0, which is not recommended.")
  }
  new_unif_ellipsoid(
    unit_log_fn = NULL,
    n_dim = NULL,
    enlarge = enlarge,
    max_loop = getOption("ernest.max_loop", 1e6L)
  )
}

#' @export
#' @noRd
format.unif_ellipsoid <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_text("Center: {pretty(x$cache$center %||% 'Undefined')}")
    cli::cli_text("Log Volume: {pretty(x$cache$log_volume %||% -Inf)}")
    cli::cli_text("Enlargement: {x$enlarge}")
  })
}

#' Create a new unif_ellipsoid LRPS
#'
#' Internal constructor for uniform ellipsoid LRPS objects.
#'
#' @param unit_log_fn Function to compute log-likelihood in unit space.
#' @param n_dim Integer. Number of dimensions.
#' @param max_loop Integer. Maximum proposal attempts.
#' @param cache Optional cache environment.
#'
#' @return An LRPS specification, a list with class
#' `c("unif_ellipsoid", "ernest_lrps")`.
#' @noRd
new_unif_ellipsoid <- function(
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
    c("center", "shape", "inv_sqrt_shape", "log_volume")
  ))
  if (!has_ell && (is_integerish(n_dim) && n_dim > 0)) {
    ell <- BoundingEllipsoid(matrix(double(), nrow = 0, ncol = n_dim))
    env_bind(
      cache,
      center = ell$center,
      shape = ell$shape,
      inv_sqrt_shape = ell$inv_sqrt_shape,
      log_volume = ell$log_vol
    )
  }

  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    enlarge = enlarge,
    .class = "unif_ellipsoid"
  )
}

#' @rdname propose
#' @export
propose.unif_ellipsoid <- function(
  x,
  original = NULL,
  criterion = -Inf
) {
  if (is.null(original)) {
    NextMethod(x)
  } else {
    res <- propose_ellipsoid(
      unit_log_fn = x$unit_log_fn,
      criterion = criterion,
      center = x$cache$center,
      inv_sqrt_shape = x$cache$inv_sqrt_shape,
      enlarge = x$enlarge,
      max_loop = x$max_loop
    )
    env_poke(x$cache, "n_call", x$cache$n_call + res$n_call)
    res
  }
}

#' Generate a new point in an ellipsoid
#'
#' @param unit_log_fn Function to compute log-likelihood in unit space.
#' @param criterion Double scalar. A log-likelihood value that proposed points
#' must satisfy.
#' @param center Vector. The center of the ellipsoid.
#' @param inv_sqrt_shape Matrix. The inverse square root of the shape matrix.
#' @param enlarge Double. Enlargement factor for the ellipsoid.
#' @param max_loop Positive integer. Maximum number of attempts to generate
#' a point.
#'
#' @returns A list with:
#' * `unit`: Vector of proposed points in the prior space.
#' * `log_lik`: Numeric vector of log-likelihood values for the proposed.
#' * `n_call`: Number of calls made to `unit_log_fn` during the proposal.
#' @noRd
propose_ellipsoid <- function(
  unit_log_fn,
  criterion,
  center,
  inv_sqrt_shape,
  enlarge,
  max_loop
) {
  n_dim <- length(center)
  radius <- enlarge^(1 / n_dim)
  proposal <- double(n_dim)
  for (i in seq_len(max_loop)) {
    proposal <- uniformly::runif_in_sphere(1, n_dim, r = radius)
    proposal <- drop(tcrossprod(proposal, inv_sqrt_shape)) + center
    if (any(proposal < 0) || any(proposal > 1)) {
      next
    }
    log_lik <- unit_log_fn(proposal)
    if (log_lik >= criterion) {
      return(list(
        unit = proposal,
        log_lik = log_lik,
        n_call = i
      ))
    }
  }
  list(unit = NULL, log_lik = NULL, n_call = max_loop)
}

#' @rdname update_lrps
#' @export
update_lrps.unif_ellipsoid <- function(x, unit = NULL, ...) {
  if (is.null(unit)) {
    return(do.call(new_unif_ellipsoid, as.list(x)))
  }
  ell <- BoundingEllipsoid(unit)
  if (ell$error != 0L) {
    cli::cli_warn(
      "Ellipsoid fitting returned an error code ({ell$error})."
    )
  }
  env_bind(
    x$cache,
    center = ell$center,
    shape = ell$shape,
    inv_sqrt_shape = ell$inv_sqrt_shape,
    log_volume = ell$log_vol
  )
  do.call(new_unif_ellipsoid, as.list(x))
}
