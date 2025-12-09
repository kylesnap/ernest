#' Generate samples from a p-norm ball
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Propose new live points from a multidimensional p-norm ball centered
#' on a randomly selected live point. The radius of the balls are set
#' such that each ball encompasses at least two live points (i.e., the
#' largest empty p-ball).
#'
#' @param method Sets the distance measure to be used. This must be one of
#' `"euclidean"` or `"maximum"`.
#' @param bootstrap Sets the number of bootstrap replications to use when estimating
#' the largest empty p-ball. If `NULL`, jacknife estimation is used instead.
#'
#' @returns A list with class `c("mini_ball", "ernest_lrps")`. Use with
#' [ernest_sampler()] to specify nested sampling behaviour.
#'
#' @details The p-norm naturally defines a distance that makes the vector space
#' a metric space. For two points `x` and `y`, their distance in \eqn{L^p}
#' space is given by \deqn{||x-y||_p = \sigma{(x_i - y_i)^p}^{1/p}}.
#'
#' The p-norm ball is the set of all vectors such that the distance between
#' itself and the ball's centre `c` is less than the radius `r`. The distance
#' `r` is updated throughout a run to ensure that at least one other live point
#' is contained within the ball.
#'
#' @references
#' Buchner, J. (2014). A Statistical Test for Nested Sampling Algorithms.
#' Statistics and Computing, 26(1–2), 383–392.
#' \doi{10.1007/s11222-014-9512-y}
#'
#' Buchner, J. (2019). Collaborative Nested Sampling: Big Data versus Complex
#' Physical Models. Publications of the Astronomical Society of the Pacific,
#' 131(1004), 108005. \doi{10.1088/1538-3873/aae7fc}
#'
#' @section Status:
#' This LRPS is experimental and has not been extensively validated across
#' different nested sampling problems. You are encouraged to use it, but please
#' exercise caution interpretting results and report any issues or unexpected
#' behaviour.
#'
#' @family ernest_lrps
#' @examples
#' data(example_run)
#' euclid_balls <- mini_balls(method = "euclidean")
#'
#' suprenum_balls <- mini_balls(method = "maximum")
#'
#' ernest_sampler(
#'   example_run$log_lik_fn,
#'   example_run$prior,
#'   sampler = euclid_balls
#' )
#' @keywords internal
#' @export
mini_balls <- function(method = c("euclidean", "maximum"), bootstrap = NULL) {
  new_mini_balls(
    unit_log_fn = NULL,
    n_dim = NULL,
    method = method,
    bootstrap = bootstrap,
    max_loop = getOption("ernest.max_loop", 1e6L)
  )
}

#' @export
#' @noRd
format.mini_balls <- function(x, ...) {
  boot <- if (is.null(x$bootstrap)) "Disabled" else x$bootstrap
  radius <- env_cache(x$cache, "radius", -Inf)
  if (radius == -Inf) {
    radius <- "Undefined"
  }
  glue::glue(
    "{format.ernest_lrps(x)}",
    "Method: {x$method}",
    "Bootstrap: {boot}",
    "Radius: {radius}",
    .sep = "\n"
  )
}

#' Create a new mini_ball LRPS
#'
#' Internal constructor for minimal p-norm ball LRPS objects.
#'
#' @param unit_log_fn Function to compute log-likelihood in unit space.
#' @param n_dim Integer. Number of dimensions.
#' @param method Character. p-norm to use.
#' @param bootstrap Integer or NULL. Bootstrap replications.
#' @param max_loop Integer. Maximum proposal attempts.
#' @param cache Optional cache environment.
#'
#' @return An LRPS specification, a list with class
#' `c("mini_ball", "ernest_lrps")`.
#' @noRd
new_mini_balls <- function(
  unit_log_fn = NULL,
  n_dim = NULL,
  method = c("euclidean", "maximum"),
  bootstrap = NULL,
  max_loop = 1e6L,
  cache = NULL
) {
  method <- arg_match(method)
  check_number_whole(bootstrap, min = 1, allow_null = TRUE)
  cache <- cache %||% new_environment()
  env_cache(cache, "radius", -Inf)
  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    method = as.character(method),
    bootstrap = if (!is.null(bootstrap)) as.integer(bootstrap) else NULL,
    .class = "mini_balls"
  )
}

#' @rdname propose
#' @export
propose.mini_balls <- function(x, original = NULL, criterion = -Inf) {
  if (is.null(original)) {
    NextMethod(x)
  } else {
    radius <- env_cache(x$cache, "radius", -Inf)
    if (!is.finite(radius)) {
      cli::cli_warn(
        "`x` does not have a valid radius to sample within."
      )
      return(propose.ernest_lrps(x, original = NULL, criterion))
    }
    res <- propose_pball(
      unit_log_fn = x$unit_log_fn,
      criterion = criterion,
      original = original,
      radius = x$cache$radius,
      method = x$method,
      max_loop = x$max_loop
    )
    env_poke(x$cache, "n_call", x$cache$n_call + res$n_call)
    res
  }
}

#' Generate a new point in a p-norm ball
#'
#' @param unit_log_fn Function to compute log-likelihood in unit space.
#' @param criterion Double scalar. A log-likelihood value that proposed points
#' must satisfy.
#' @param original Vector. The center of the ball.
#' @param radius Double. The radius of the ball.
#' @param method Character. The p-norm to use.
#' @param max_loop Positive integer. Maximum number of attempts to generate a
#' point.
#'
#' @returns A list with:
#' * `unit`: Vector of proposed points in the prior space.
#' * `log_lik`: Numeric vector of log-likelihood values for the proposed.
#' * `n_call`: Number of calls made to `unit_log_fn` during the proposal.
#' @noRd
propose_pball <- function(
  unit_log_fn,
  criterion,
  original,
  radius,
  method,
  max_loop
) {
  n_dim <- length(original)
  proposal <- double(n_dim)
  sample_gen <- switch(
    method,
    "maximum" = expr(stats::runif(!!n_dim, min = 0, max = !!radius)),
    "euclidean" = expr(uniformly::runif_in_sphere(1, !!n_dim, r = !!radius))
  )
  for (i in seq_len(max_loop)) {
    proposal <- drop(eval_bare(sample_gen) + original)
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
update_lrps.mini_balls <- function(x, unit = NULL, ...) {
  if (is.null(unit)) {
    return(do.call(new_mini_balls, as.list(x)))
  }

  # Calculate distances
  method <- switch(x$method, "euclidean" = "e", "maximum" = "c")
  fit <- MiniBall(X = unit, n_bootstraps = x$bootstrap %||% 0L, method = method)
  radius <- if (fit$max_radius <= 0 || !is.finite(fit$max_radius)) {
    cli::cli_warn(c(
      "`radius` must be a finite, positive value, not {fit$max_radius}.",
      "!" = "Falling-back to uniform hypercube sampling."
    ))
    -Inf
  } else {
    fit$max_radius
  }
  env_poke(x$cache, "radius", radius)
  do.call(new_mini_balls, as.list(x))
}
