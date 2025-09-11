#' Uniformly sample from the ellipsoid hull of live points
#'
#' Generate new live points from the ellipsoid hull or spanning ellipsoid of
#' the current set of live points. Individual points are generated through
#' rejection sampling within the ellipsoid, while [update_lrps()] updates the
#' bounding ellipsoid using [cluster::ellipsoidhull].
#'
#' @param scale A double, used to inflate the volume of the ellipsoid before
#' sampling from it.
#'
#' @returns A list with class `c("unif_ellipsoid", "ernest_lrps")`. Can be used
#' with [ernest_sampler()] to specify the sampling behaviour of a nested
#' sampling run.
#'
#' @export
unif_ellipsoid <- function(scale = 1.0) {
  check_installed("cluster", reason = "finding convex hulls")
  new_unif_ellipsoid(
    unit_log_fn = NULL,
    n_dim = NULL,
    scale = scale,
    max_loop = getOption("ernest.max_loop", 1e6L)
  )
}

#' @export
#' @noRd
format.unif_ellipsoid <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_text("Uniform Ellipsoid LRPS {.cls {class(x)}}")
    cli::cli_text("No. Dimensions: {x$n_dim %||% 'Uninitialized'}")
    cli::cli_text("No. Calls Since Update: {x$cache$n_call %||% 0L}")
    if (all(env_has(x$cache, c("loc", "volume")))) {
      cli::cli_dl(c(
        "Centre" = "{pretty(x$cache$loc)}",
        "Volume" = "{pretty(x$cache$volume)}",
        "Scaling Factor" = if (x$scale != 1) "{x$scale}" else NULL
      ))
    }
  })
}

#' Create a new unif_ellipsoid LRPS
#'
#' Internal constructor for uniform ellipsoid LRPS objects.
#'
#' @param unit_log_fn Function for computing log-likelihood in unit space.
#' @param n_dim Integer. Number of dimensions.
#' @param max_loop Integer. Maximum number of proposal attempts.
#' @param cache Optional cache environment.
#'
#' @return An LRPS specification, a list with class
#' `c("rwmh_cube", "ernest_lrps")`.
#' @noRd
new_unif_ellipsoid <- function(
  unit_log_fn = NULL,
  n_dim = NULL,
  max_loop = 1e6L,
  cache = NULL,
  scale = 1.0
) {
  check_number_decimal(scale, min = 1, allow_infinite = FALSE)
  cache <- cache %||% new_environment()
  if (is_integerish(n_dim) && n_dim > 0) {
    if (n_dim <= 1) {
      cli::cli_abort("`n_dim` must be larger than 1.")
    }
    env_cache(cache, "chol_precision", diag(n_dim))
    env_cache(cache, "loc", rep(0.5, n_dim))
    env_cache(cache, "d2", n_dim / 4)
    env_cache(
      cache,
      "volume",
      pi^(n_dim / 2) / gamma(n_dim / 2 + 1) * (sqrt(n_dim) / 2)^n_dim
    )
  }
  env_poke(cache, "n_call", 0L)
  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    scale = scale,
    .class = "unif_ellipsoid"
  )
}

#' @rdname propose
#' @export
propose.unif_ellipsoid <- function(x, original = NULL, criteria = NULL) {
  if (is.null(original)) {
    NextMethod(x, criteria)
  } else {
    res <- EllipsoidImpl(
      unit_log_fn = x$unit_log_fn,
      criterion = criteria,
      chol_precision = x$cache$chol_precision,
      loc = x$cache$loc,
      d2 = x$cache$d2,
      max_loop = x$max_loop
    )
    if (isTRUE(getOption("ernest_logging", FALSE))) {
      log4r::debug(
        x$cache$logger,
        method = "ellipsoid",
        original = original,
        new = res$unit,
        ncall = res$n_call
      )
    }
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
      scaled_d2 <- ellipsoid$d2 * x$scale^(2 / x$n_dim)
      env_poke(x$cache, "chol_precision", chol_precision)
      env_poke(x$cache, "loc", ellipsoid$loc)
      env_poke(x$cache, "d2", scaled_d2)
      env_poke(x$cache, "volume", cluster::volume(ellipsoid))
    },
    error = function(cnd) {
      cli::cli_warn(
        "Sampling from the unit sphere after encountering a rebounding error.",
        parent = cnd
      )
      env_unbind(x$cache, c("chol_precision", "loc", "d2"))
    }
  )
  if (isTRUE(getOption("ernest_logging", FALSE))) {
    log4r::info(
      x$cache$logger,
      method = "ellipsoid",
      chol_precision = x$cache$chol_precision %||% 0,
      loc = x$cache$loc %||% 0,
      d2 = x$cache$d2 %||% 0,
      unit = unit
    )
  }
  do.call(new_unif_ellipsoid, as.list(x))
}
