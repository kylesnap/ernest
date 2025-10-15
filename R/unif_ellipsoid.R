#' Generate samples from the spanning ellipsoid
#'
#' Uses the bounding ellipsoid of live points to define the prior region for new
#' samples. Effective for unimodal, roughly Gaussian posteriors.
#'
#' @param enlarge Double, larger or equal to 1. Factor to inflate the bounding
#' ellipsoid's volume before sampling (see Details).
#'
#' @details Nested likelihood contours rarely form perfect ellipses, so sampling
#' from the spanning ellipsoid without enlargement may exclude valid regions.
#' This can bias proposals toward the ellipsoid centre and overestimate
#' evidence. Setting `enlarge = 1` will produce a warning.
#'
#' Ellipsoids are stored in the LRPS cache in quadric representation. That is,
#' an ellipsoid with center \eqn{c} and shape matrix \eqn{Q} contains points
#' \eqn{x \in R^n} that satisfy the inequality
#' \deqn{(x - c) Q^-1 (x - c)^T \leq 1}
#' The square root of the precision matrix \eqn{Q}, scaled by `enlarge`, is
#' also stored for efficient point generation.
#'
#' The covariance matrix of the points is used estimate \eqn{Q}. In exceptional
#' cases (e.g. perfect collinearity), this matrix may be singular. Should this
#' occur, the covariance matrix is reconditioned by adjusting its eigenvalues.
#' Should this also fail, the algorithm falls back to sampling from the
#' circumscribed sphere bounding the unit hypercube.
#'
#' @returns A list with class `c("unif_ellipsoid", "ernest_lrps")`. Use with
#' [ernest_sampler()] to specify nested sampling behaviour.
#'
#' @references
#' Feroz, F., Hobson, M. P., Bridges, M. (2009) MULTINEST: An Efficient and
#' Robust Bayesian Inference Tool for Cosmology and Particle Physics. Monthly
#' Notices of the Royal Astronomical Society. 398(4), 1601–1614.
#' <https://doi.org/10.1111/j.1365-2966.2009.14548.x>
#'
#' Mukherjee, P., Parkinson, D., & Liddle, A. R. (2006). A Nested Sampling
#' Algorithm for Cosmological Model Selection. The Astrophysical Journal,
#' 638(2), L51. <https://doi.org/10.1086/501068>
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
    cli::cli_text("uniform ellipsoid LRPS {.cls {class(x)}}")
    cli::cat_line()
    cli::cli_text("No. Dimensions: {x$n_dim %||% 'Uninitialized'}")
    if (all(env_has(x$cache, c("center", "log_volume")))) {
      cli::cli_dl(c(
        "Centre" = "{pretty(x$cache$center)}",
        "Log Volume" = "{pretty(x$cache$log_volume)}",
        "Enlargement Factor" = if (x$enlarge != 1) "{x$enlarge}" else NULL
      ))
    }
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
    enlarge = 1.0) {
  check_number_decimal(enlarge, min = 1, allow_infinite = FALSE)
  cache <- cache %||% new_environment()
  if (is_integerish(n_dim) && n_dim > 0) {
    env_cache(cache, "inverse_shape", diag(n_dim))
    env_cache(cache, "center", rep(0.5, n_dim))
    env_cache(cache, "scaled_sqrt_shape", diag(sqrt(n_dim / 4), nrow = n_dim))
    env_cache(cache, "scale", n_dim / 4)
    log_vol <- ((n_dim / 2) * log(pi)) -
      lgamma(n_dim / 2 + 1) +
      n_dim * ((1 / 2) * log(n_dim) - log(2))
    env_cache(cache, "log_volume", log_vol)
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
    criterion = -Inf,
    idx = NULL) {
  if (is.null(original)) {
    NextMethod(x)
  } else {
    res <- EllipsoidImpl(
      unit_log_fn = x$unit_log_fn,
      criterion = criterion,
      scaledInvSqrtA = x$cache$scaled_sqrt_shape,
      loc = x$cache$center,
      max_loop = x$max_loop
    )
    env_poke(x$cache, "n_call", x$cache$n_call + res$n_call)
    res
  }
}

#' @rdname update_lrps
#' @export
update_lrps.unif_ellipsoid <- function(x, unit = NULL) {
  if (is.null(unit)) {
    return(do.call(new_unif_ellipsoid, as.list(x)))
  }
  try_fetch(
    {
      ellipsoid <- BoundingEllipsoid(unit)
      if (ellipsoid$log_vol == -Inf) {
        cli::cli_abort("Encountered a fatal numerical error.")
      } else if (ellipsoid$error != 0L) {
        cli::cli_warn(
          "Ellipsoid fitting returned an error code ({ellipsoid$error})."
        )
      }

      env_poke(x$cache, "inverse_shape", ellipsoid$inverse_shape)
      env_poke(x$cache, "center", ellipsoid$center)
      env_poke(
        x$cache,
        "scaled_sqrt_shape",
        ellipsoid$sqrt_shape * sqrt(x$enlarge)
      )
      env_poke(
        x$cache,
        "log_volume",
        ellipsoid$log_vol + x$n_dim * log(x$enlarge) / 2
      )
    },
    error = function(cnd) {
      cli::cli_warn(
        "Sampling from the unit sphere after encountering a rebounding error.",
        parent = cnd
      )
      env_unbind(
        x$cache,
        c("inverse_shape", "center", "scaled_sqrt_shape", "log_volume")
      )
    }
  )
  do.call(new_unif_ellipsoid, as.list(x))
}
