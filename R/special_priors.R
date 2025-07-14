#' @srrstats {BS2.5} `special_priors` check user-entered distributional
#' parameters to ensure their validity.
#' @noRd
NULL

#' Specify a prior with normally-distributed marginals.
#'
#' A specialization of [create_prior()], where the parameter space is
#' described by independently distributed normal variables that are possibly
#' truncated.
#'
#' @inheritParams create_prior
#' @inheritParams stats::qnorm
#' @param lower,upper Numeric vectors, representing the values to retain from
#' each distribution. Changing these from default will truncate the distribution.
#'
#' @returns A `normal_prior`, which is a subclass of `ernest_prior` with
#' an efficient implementation of the unit hypercube transformation.
#'
#' @references For the truncation routine:
#' Nadarajah, S., & Kotz, S. (2006).
#' R Programs for Truncated Distributions. Journal of Statistical Software,
#' Code Snippets, 16(2), 1–8. <https://doi.org/10.18637/jss.v016.c02>
#'
#' @seealso [create_prior()] for a richer explanation of the `ernest_prior` object.
#' @family special priors
#' @importFrom vctrs vec_cast
#' @export
#' @examples
#' prior <- create_normal_prior(3)
#' prior$fn(c(0.25, 0.5, 0.75))
create_normal_prior <- function(
  n_dim,
  mean = 0,
  sd = 1,
  varnames = NULL,
  lower = -Inf,
  upper = Inf
) {
  mean <- vec_cast(mean, double())
  sd <- vec_cast(sd, double())
  if (any(sd <= 0)) {
    cli_abort("`sd` of a normal distribution must be non-negative.")
  }
  default_names <- is.null(varnames)

  params <- check_prior_params(n_dim, varnames %||% "N", lower, upper)
  dparams <- vctrs::vec_recycle_common(
    "mean" = mean,
    "sd" = sd,
    .size = params$n_dim
  )
  if (default_names) {
    params$varnames <- sprintf(
      "N(%s, %s)",
      round(dparams$mean, 3),
      round(dparams$sd^2, 3)
    ) |>
      make.unique()
  }

  exp_p <- if (any(params$lower != -Inf) || any(params$upper != Inf)) {
    f_lwr <- stats::pnorm(params$lower, dparams$mean, dparams$sd)
    f_upr <- stats::pnorm(params$upper, dparams$mean, dparams$sd)
    expr(!!f_lwr + p * !!(f_upr - f_lwr))
  } else {
    expr(p)
  }

  p <- NULL
  fn <- new_function(
    exprs(p = ),
    expr(qnorm(p = !!exp_p, mean = !!dparams$mean, sd = !!dparams$sd))
  )
  batched_fn <- \(x) {
    if (is.matrix(x)) {
      t(apply(x, 1, fn))
    } else {
      fn(x)
    }
  }

  new_ernest_prior(
    batched_fn,
    params$n_dim,
    params$varnames,
    lower = params$lower,
    upper = params$upper,
    class = "normal_prior"
  )
}

#' Specify a prior with Student's t-distributed marginals
#'
#' A specialization of [create_prior()], where the parameter space is
#' described by independently distributed Student's t marginals that are possibly
#' truncated.
#'
#' @inheritParams create_normal_prior
#' @inheritParams distributional::dist_student_t
#'
#' @returns A `student_t_prior`, which is a subclass of `ernest_prior` with
#' an efficient implementation of the unit hypercube transformation.
#'
#' @details
#' The parameters used to specify this prior are for the location-scale
#' t-distribution with location $\mu$, scale $\sigma$, and degrees of freedom
#' $\nu$. If the random variable $X$ is Student-T distributed, then
#' $X \sym \mu + \sigma T$, where $T$ follows the Student-T distribution
#' with $\nu$ degrees of freedom.
#'
#' @inherit create_normal_prior seealso references
#'
#' @seealso [create_prior()] for a richer explanation of the `ernest_prior` object.
#' @family special priors
#' @export
#' @examples
#' prior <- create_t_prior(3, df = c(1,2,5), mu = c(0,1,2), sigma = c(1,2,3))
#' prior$fn(c(0.25, 0.5, 0.75))
create_t_prior <- function(
  n_dim,
  df,
  mu = 0,
  sigma = 1,
  varnames = NULL,
  lower = -Inf,
  upper = Inf
) {
  df <- vec_cast(df, numeric())
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if (any(df <= 0)) {
    cli_abort("`df` of Student's t distribution must be non-negative.")
  }
  if (any(sigma <= 0)) {
    cli_abort("`sigma` of Student's t distribution must be non-negative.")
  }
  default_names <- is.null(varnames)

  params <- check_prior_params(n_dim, varnames %||% "T", lower, upper)
  dparams <- vctrs::vec_recycle_common(
    "df" = df,
    "mu" = mu,
    "sigma" = sigma,
    .size = params$n_dim
  )
  if (default_names) {
    params$varnames <- sprintf(
      "T(%s, %s, %s)",
      round(dparams$df, 3),
      round(dparams$mu, 3),
      round(dparams$sigma, 3)
    ) |>
      make.unique()
  }

  exp_p <- if (any(params$lower != -Inf) || any(params$upper != Inf)) {
    f_lwr <- stats::pt(
      (params$lower - dparams$mu) / dparams$sigma,
      dparams$df
    )
    f_upr <- stats::pt(
      (params$upper - dparams$mu) / dparams$sigma,
      dparams$df
    )
    expr(!!f_lwr + p * !!(f_upr - f_lwr))
  } else {
    expr(p)
  }

  p <- NULL
  fn <- new_function(
    exprs(p = ),
    expr(
      stats::qt(p = !!exp_p, df = !!dparams$df) * !!dparams$sigma + !!dparams$mu
    )
  )
  batched_fn <- \(x) {
    if (is.matrix(x)) {
      t(apply(x, 1, fn))
    } else {
      fn(x)
    }
  }

  new_ernest_prior(
    batched_fn,
    params$n_dim,
    params$varnames,
    lower = params$lower,
    upper = params$upper,
    class = "student_t_prior"
  )
}

#' Specify a prior with Cauchy-distributed marginals
#'
#' A specialization of [create_prior()], where the parameter space is
#' described by independently distributed Cauchy marginals that are possibly
#' truncated.
#'
#' @inheritParams create_normal_prior
#' @inheritParams stats::qcauchy
#'
#' @returns A `cauchy_prior`, which is a subclass of `ernest_prior` with
#' an efficient implementation of the unit hypercube transformation.
#'
#' @inherit create_normal_prior seealso references
#'
#' @family special priors
#' @export
#' @examples
#' prior <- create_cauchy_prior(3, location = c(0, 0, -2), scale = c(0.5, 1, 1))
#' prior$fn(c(0.25, 0.5, 0.75))
create_cauchy_prior <- function(
  n_dim,
  location = 0,
  scale = 1,
  varnames = NULL,
  lower = -Inf,
  upper = Inf
) {
  location <- vec_cast(location, to = double())
  scale <- vec_cast(scale, to = double())
  if (any(scale <= 0)) {
    cli_abort("`scale` of the Cauchy distribution must be non-negative.")
  }
  default_names <- is.null(varnames)

  params <- check_prior_params(n_dim, varnames %||% "Cauchy", lower, upper)
  dparams <- vctrs::vec_recycle_common(
    "location" = location,
    "scale" = scale,
    .size = params$n_dim
  )
  if (default_names) {
    params$varnames <- sprintf(
      "Cauchy(%s, %s)",
      round(dparams$location, 3),
      round(dparams$scale, 3)
    ) |>
      make.unique()
  }

  exp_p <- if (any(params$lower != -Inf) || any(params$upper != Inf)) {
    f_lwr <- stats::pcauchy(params$lower, dparams$location, dparams$scale)
    f_upr <- stats::pcauchy(params$upper, dparams$location, dparams$scale)
    expr(!!f_lwr + p * !!(f_upr - f_lwr))
  } else {
    expr(p)
  }

  p <- NULL
  fn <- new_function(
    exprs(p = ),
    expr(
      stats::qcauchy(
        p = !!exp_p,
        location = !!dparams$location,
        scale = !!dparams$scale
      )
    )
  )
  batched_fn <- \(x) {
    if (is.matrix(x)) {
      t(apply(x, 1, fn))
    } else {
      fn(x)
    }
  }

  new_ernest_prior(
    batched_fn,
    params$n_dim,
    params$varnames,
    lower = params$lower,
    upper = params$upper,
    class = "cauchy_prior"
  )
}

#' Specify a prior with Uniformly-distributed marginals
#'
#' A specialization of [create_prior()], where the parameter space is
#' described by independently distributed uniform marginals.
#'
#' @inheritParams create_normal_prior
#' @param lower,upper Vectors of limits of the distribution.
#'
#' @returns A `uniform_prior`, which is a subclass of `ernest_prior` with
#' an efficient implementation of the unit hypercube transformation.
#'
#' @inherit create_normal_prior seealso
#'
#' @family special priors
#' @export
#' @examples
#' prior <- create_uniform_prior(2, lower = c(3, -2), upper = c(5, 4))
#' prior$fn(c(0.33, 0.67))
create_uniform_prior <- function(
  n_dim,
  lower = 0,
  upper = 1,
  varnames = NULL
) {
  default_names <- is.null(varnames)
  params <- check_prior_params(n_dim, varnames %||% "Uniform", lower, upper)
  if (default_names) {
    params$varnames <- sprintf(
      "Uniform(%s, %s)",
      round(params$lower, 3),
      round(params$upper, 3)
    ) |>
      make.unique()
  }

  p <- NULL
  fn <- new_function(
    exprs(p = ),
    expr(
      stats::qunif(p = p, min = !!params$lower, max = !!params$upper)
    )
  )
  batched_fn <- \(x) {
    if (is.matrix(x)) {
      t(apply(x, 1, fn))
    } else {
      fn(x)
    }
  }

  new_ernest_prior(
    batched_fn,
    params$n_dim,
    params$varnames,
    lower = params$lower,
    upper = params$upper,
    class = "uniform_prior"
  )
}
