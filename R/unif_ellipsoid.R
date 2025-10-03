#' Generate samples from the spanning ellipsoid
#'
#' Uses the bounding ellipsoid of live points to define the prior region for new
#' samples. Effective for unimodal, roughly Gaussian posteriors. The ellipsoid
#' is found using [cluster::ellipsoidhull] and its volume is inflated by a
#' constant enlargement factor.
#'
#' @param enlarge Double, larger or equal to 1. Factor to inflate the bounding
#' ellipsoid's volume before sampling (see Details).
#'
#' @details Nested likelihood contours rarely form perfect ellipses, so sampling
#' from the spanning ellipsoid without enlargement may exclude valid regions.
#' This can bias proposals toward the ellipsoid centre and overestimate
#' evidence. Setting `enlarge = 1` will produce a warning.
#'
#' If [cluster::ellipsoidhull] fails to converge, `unif_ellipsoid` falls back to
#' sampling from the circumscribed sphere bounding the unit hypercube.
#'
#' @returns A list with class `c("unif_ellipsoid", "ernest_lrps")`. Use with
#' [ernest_sampler()] to specify nested sampling behaviour.
#'
#' @references
#' Barbary, K. (2015). nestle: Pure Python,
#' MIT-Licensed Implementation of Nested Sampling Algorithms for Evaluating
#' Bayesian Evidence. (Computer software, Version 0.2).
#' <https://github.com/kbarbary/nestle>.
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
  check_installed("cluster", reason = "finding convex hulls")
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
    if (all(env_has(x$cache, c("loc", "log_volume")))) {
      cli::cli_dl(c(
        "Centre" = "{pretty(x$cache$loc)}",
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
#' @return An LRPS specification, a list with class `c("rwmh_cube", "ernest_lrps")`.
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
  if (is_integerish(n_dim) && n_dim > 0) {
    if (n_dim <= 1) {
      cli::cli_abort("`n_dim` must be larger than 1.")
    }
    env_cache(cache, "chol_precision", diag(n_dim))
    env_cache(cache, "loc", rep(0.5, n_dim))
    env_cache(cache, "d2", n_dim / 4)
    log_vol <- ((n_dim / 2) * log(pi)) -
      lgamma(n_dim / 2 + 1) +
      n_dim * ((1 / 2) * log(n_dim) - log(2))
    env_cache(cache, "log_volume", log_vol)
  }
  env_poke(cache, "n_call", 0L)
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
  idx = NULL
) {
  if (is.null(original)) {
    NextMethod(x)
  } else {
    res <- EllipsoidImpl(
      unit_log_fn = x$unit_log_fn,
      criterion = criterion,
      chol_precision = x$cache$chol_precision,
      loc = x$cache$loc,
      d2 = x$cache$d2,
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
      ellipsoid <- suppressWarnings(cluster::ellipsoidhull(unit))
      if (!ellipsoid$conv) {
        cli::cli_abort(c(
          "Failed to estimate the spanning ellipsoid.",
          "x" = if (ellipsoid$ierr != 0) "Error thrown by the Fortran routine."
        ))
      }
      chol_precision <- chol(solve(ellipsoid$cov))
      enlarged_d2 <- ellipsoid$d2 * x$enlarge^(2 / x$n_dim)
      env_poke(x$cache, "chol_precision", chol_precision)
      env_poke(x$cache, "loc", ellipsoid$loc)
      env_poke(x$cache, "d2", enlarged_d2)
      env_poke(x$cache, "log_volume", cluster::volume(ellipsoid, log = TRUE))
    },
    error = function(cnd) {
      cli::cli_warn(
        "Sampling from the unit sphere after encountering a rebounding error.",
        parent = cnd
      )
      env_unbind(x$cache, c("chol_precision", "loc", "d2"))
    }
  )
  do.call(new_unif_ellipsoid, as.list(x))
}
