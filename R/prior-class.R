#' Specify a prior distribution for nested sampling
#'
#' Create a prior specification for ernest using an R function that transforms
#' coordinates from the unit cube space into coordinates in the prior
#' distribution (see Details).
#'
#' @param fn A function that takes a vector of probabilities `p` and returns
#' a vector of quantiles.
#' @param n_dim The number of dimensions for the prior distribution.
#' @param varnames Optional names for the variables in the prior distribution.
#' @param lower Optional lower bounds for the prior distribution.
#' @param upper Optional upper bounds for the prior distribution.
#'
#' @returns An object of type `ernest_prior` that represents the prior
#' distribution, including:
#' * `fn`: The function that performs the transformation from unit cube
#' coordinates to coordinates in the prior space.
#' * `n_dim`: The number of dimensions in the prior space.
#' * `varnames`: Names for the variables in the prior distribution.
#' * `lower`: Lower bounds for the prior distribution.
#' * `upper`: Upper bounds for the prior distribution.
#'
#' @details
#' Nested sampling with ernest requires a prior transformation function that
#' takes in a vector of values between 0 and 1, and returns a same-length vector
#' of parameters expressed in the scale of the model's prior distribution. When
#' calling `create_prior`, ernest will test `fn` by presenting it with 1000
#' `n_dim`-length vectors generated from [runif()]. These checks are passed if:
#' * Every input returns a vector of `n_dim` finite values.
#' * Every input returns a vector that respects the `lower` and `upper` bounds,
#' if provided.
#'
#' Users are encouraged to run additional checks on the behaviour of the prior
#' through calling the `fn` object bound to the produced `ernest_prior`.
#'
#' @examples
#' # 3D uniform prior in the range [-10, 10]
#' unif <- \(x) { -10 + x * 20 }
#' prior <- create_prior(unif, n_dim = 3, lower = -10, upper = 10)
#'
#' # `ernest_prior` wraps `fn` in guards to prevent unexpected behaviour.
#' prior$fn(c(0.25, 0.5, 0.75)) # OK
#' try(prior$fn(c(0.33, 0.67))) # Input length is only length 3.
#'
#' # A prior for a simple linear regression model
#' lm_f <- function(theta) {
#'   beta_0 <- qnorm(theta[1], mean = 0, sd = 10) # (Beta0, Beta1) ~ N(0, 10)
#'   beta_1 <- qnorm(theta[1], mean = 0, sd = 10) # Sigma ~ Exp(1)
#'   sigma <- qexp(theta[1], rate = 1)
#'   c(beta_0, beta_1, sigma)
#' }
#' create_prior(
#'   lm_f,
#'   n_dim = 3,
#'   varnames = c("beta_0", "beta_1", "sigma"),
#'   lower = c(-Inf, -Inf, 0)
#' )
#'
#' # A normal prior with hyperprior parameters
#' hier_f <- function(theta) {
#'   mu <- qnorm(theta[1], mean = 5) # mu ~ N(5, 1)
#'   sigma <- 10 ^ qunif(theta[2], min = -1, max = 1) # log10(sigma) ~ U[-1, 1]
#'
#'   x <- qnorm(theta[3], mu, sigma) # X ~ N(mu, sigma)
#'   c(mu, sigma, x)
#' }
#' create_prior(
#'   hier_f,
#'   n_dim = 3,
#'   varnames = c("mu", "sigma", "x"),
#'   lower = c(-Inf, 0, -Inf)
#' )
#'
#' # ernest will catch mistakes when provided with bounds
#' try(create_prior(
#'   hier_f,
#'   n_dim = 3,
#'   varnames = c("mu", "sigma", "x"),
#'   lower = c(-Inf, -Inf, -1)
#' ))
#' @export
create_prior <- function(
  fn,
  n_dim,
  varnames = NULL,
  lower = NULL,
  upper = NULL
) {
  p <- NULL
  quantile <- call2(fn, expr(p))
  prior_fn <- new_function(
    exprs(p = ),
    expr({
      if (!is_double(p, n = !!n_dim)) {
        cli::cli_abort("`p` must be a double vector of length {!!n_dim}.")
      }
      !!quantile
    })
  )

  new_ernest_prior(
    prior_fn,
    n_dim,
    varnames = varnames,
    lower = lower,
    upper = upper
  )
}

#' Specify a common prior distribution
#'
#' Create a specialized version of [create_prior()] for prior spaces
#' that are marginally independent and share a common marginal distribution.
#'
#' @inheritParams stats::qnorm
#' @inheritParams create_prior
#'
#' @returns A special instance of `ernest_prior` with an efficient implementation
#' of the prior transformation function.
#'
#' @references For the truncation routine:
#' Nadarajah, S., & Kotz, S. (2006).
#' R Programs for Truncated Distributions. Journal of Statistical Software,
#' Code Snippets, 16(2), 1–8. <https://doi.org/10.18637/jss.v016.c02>
#'
#' @seealso [create_prior()] for a richer explanation of the `ernest_prior` object.
#' @rdname create_special_prior
#' @aliases create_special_prior
#' @export
#' @examples
#' prior <- create_normal_prior(3)
#' prior$fn(c(0.25, 0.5, 0.75))
#'
#' bound_prior <- create_normal_prior(3, lower = -1, upper = 1)
#' bound_prior$fn(c(0.25, 0.5, 0.75))
#'
#' # A prior for a simple linear regression model
#' create_t_prior(
#'  3,
#'  df = 3,
#'  varnames = c("beta0", "beta1", "sigma"),
#'  lower = c(-Inf, -Inf, 0)
#' )
create_normal_prior <- function(
  n_dim,
  mean = 0,
  sd = 1,
  varnames = NULL,
  lower = -Inf,
  upper = Inf
) {
  if (any(sd <= 0)) {
    stop_input_type(sd, "must be larger than zero")
  }
  if (identical(c(lower, upper), c(-Inf, Inf))) {
    lower <- NULL
    upper <- NULL
  }
  args <- vctrs::vec_recycle_common(
    "mean" = mean,
    "sd" = sd,
    "lower" = lower,
    "upper" = upper,
    .size = n_dim
  )
  if (is.null(varnames)) {
    varnames <- sprintf(
      "N(%s, %s)",
      round(args$mean, 3),
      round(args$sd^2, 3)
    )
  }

  quantile <- expr(stats::qnorm(p = p, mean = !!args$mean, sd = !!args$sd))

  if (!is_empty(args$lower) || !(is_empty(args$upper))) {
    F_lwr <- stats::pnorm(q = args$lower, mean = args$mean, sd = args$sd)
    F_upr <- stats::pnorm(q = args$upper, mean = args$mean, sd = args$sd)
    quantile <- call_modify(quantile, p = expr(!!F_lwr + p * !!(F_upr - F_lwr)))
  }

  p <- NULL
  prior <- new_function(
    exprs(p = ),
    expr({
      if (!is_double(p, n = !!n_dim)) {
        cli::cli_abort("`p` must be a double vector of length {!!n_dim}.")
      }
      !!quantile
    })
  )

  new_ernest_prior(
    prior,
    n_dim,
    varnames,
    lower = lower,
    upper = upper,
    class = "normal_prior"
  )
}

#' @inheritParams distributional::dist_student_t
#'
#' @details
#' `create_t_prior` differs from [stats::qt()] due to the inclusion of
#' location and scale parameters. If `df` is \eqn{\nu}, then `create_t_prior`
#' returns a prior that is equivalent to \deqn{T(\nu) * \sigma + \mu}
#'
#' @rdname create_special_prior
#' @export
create_t_prior <- function(
  n_dim,
  df,
  mu = 0,
  sigma = 1,
  ncp = NULL,
  varnames = NULL,
  lower = -Inf,
  upper = Inf
) {
  if (any(df <= 0)) {
    stop_input_type(df, "must be larger than zero")
  }
  if (any(sigma <= 0)) {
    stop_input_type(sigma, "must be larger than zero")
  }
  if (identical(c(lower, upper), c(-Inf, Inf))) {
    lower <- NULL
    upper <- NULL
  }

  args <- vctrs::vec_recycle_common(
    "df" = df,
    "mu" = mu,
    "sigma" = sigma,
    "ncp" = ncp %||% 0,
    "lower" = lower,
    "upper" = upper,
    .size = n_dim
  )
  if (is.null(varnames)) {
    varnames <- sprintf(
      "T(%s, %s, %s%s)",
      round(args$df, 3),
      round(args$mu, 3),
      round(args$sigma, 3),
      ifelse(args$ncp == 0, "", paste0(", ", round(args$ncp, 3)))
    )
  }

  quantile <- expr(stats::qt(p = p, !!args$df, !!args$ncp))

  if (!is_empty(args$lower) || !(is_empty(args$upper))) {
    F_lwr <- stats::pt((args$lower - args$mu) / args$sigma, args$df, args$ncp)
    F_upr <- stats::pt((args$upper - args$mu) / args$sigma, args$df, args$ncp)
    quantile <- call_modify(quantile, p = expr(!!F_lwr + p * !!(F_upr - F_lwr)))
  }

  p <- NULL
  prior <- new_function(
    exprs(p = ),
    expr({
      if (!is_double(p, n = !!n_dim)) {
        cli::cli_abort("`p` must be a double vector of length {!!n_dim}.")
      }
      !!quantile * !!args$sigma + !!args$mu
    })
  )

  new_ernest_prior(
    prior,
    n_dim,
    varnames,
    lower = lower,
    upper = upper,
    class = "student_t_prior"
  )
}

#' @inheritParams stats::qcauchy
#'
#' @rdname create_special_prior
#' @export
create_cauchy_prior <- function(
  n_dim,
  location = 0,
  scale = 1,
  varnames = NULL,
  lower = -Inf,
  upper = Inf
) {
  if (any(scale <= 0)) {
    stop_input_type(scale, "must be larger than zero")
  }
  if (identical(c(lower, upper), c(-Inf, Inf))) {
    lower <- NULL
    upper <- NULL
  }

  args <- vctrs::vec_recycle_common(
    "location" = location,
    "scale" = scale,
    "lower" = lower,
    "upper" = upper,
    .size = n_dim
  )
  if (is.null(varnames)) {
    varnames <- sprintf(
      "Cauchy(%s, %s)",
      round(args$location, 3),
      round(args$scale, 3)
    )
  }

  quantile <- expr(
    stats::qcauchy(p = p, location = !!args$location, scale = !!args$scale)
  )

  if (!is_empty(args$lower) || !(is_empty(args$upper))) {
    F_lwr <- stats::pcauchy(args$lower, args$location, args$scale)
    F_upr <- stats::pcauchy(args$upper, args$location, args$scale)
    quantile <- call_modify(quantile, p = expr(!!F_lwr + p * !!(F_upr - F_lwr)))
  }

  p <- NULL
  prior <- new_function(
    exprs(p = ),
    expr({
      if (!is_double(p, n = !!n_dim)) {
        cli::cli_abort("`p` must be a double vector of length {!!n_dim}.")
      }
      !!quantile
    })
  )

  new_ernest_prior(
    prior,
    n_dim,
    varnames,
    lower = lower,
    upper = upper,
    class = "cauchy_prior"
  )
}

#' @rdname create_special_prior
#' @export
create_uniform_prior <- function(lower = 0, upper = 1, n_dim, varnames = NULL) {
  if (any(lower >= upper)) {
    stop_input_type(lower, "must be less than upper")
  }

  args <- vctrs::vec_recycle_common(
    "lower" = lower,
    "upper" = upper,
    .size = n_dim
  )
  if (is.null(varnames)) {
    varnames <- sprintf(
      "Uniform(%s, %s)",
      round(args$lower, 3),
      round(args$upper, 3)
    )
  }

  quantile <- expr(
    stats::qunif(p = p, min = !!args$lower, max = !!args$upper)
  )

  p <- NULL
  prior <- new_function(
    exprs(p = ),
    expr({
      if (!is_double(p, n = !!n_dim)) {
        cli::cli_abort(c(
          "`p` must be a double vector of length {!!n_dim}."
        ))
      }
      !!quantile
    })
  )

  new_ernest_prior(
    prior,
    n_dim,
    varnames,
    lower = args$lower,
    upper = args$upper,
    class = "uniform_prior"
  )
}

#' INTERNAL: Create a new prior_distribution
#'
#' Form a prior distribution object for ernest.
#'
#' @noRd
new_ernest_prior <- function(
  prior_fn,
  n_dim,
  varnames = NULL,
  lower = NULL,
  upper = NULL,
  class = NULL,
  call = caller_env()
) {
  check_function(prior_fn)
  check_number_whole(n_dim, min = 1)

  args <- vctrs::vec_recycle_common(
    "varnames" = varnames %||% "V",
    "lower" = lower,
    "upper" = upper,
    .size = n_dim
  )
  args$varnames <- make.unique(args$varnames)

  obj <- structure(
    list(
      "fn" = prior_fn,
      "n_dim" = n_dim,
      "varnames" = args$varnames,
      "lower" = args$lower,
      "upper" = args$upper
    ),
    class = c("ernest_prior", class)
  )
  validate_prior(obj, call = call)
}

#' Validation for a prior function
#' @returns obj if valid, else an error
#' @noRd
validate_prior <- function(prior, size = 1000L, call = caller_env()) {
  check_number_whole(size, min = 1)

  testmat <- matrix(stats::runif(size * prior$n_dim), ncol = prior$n_dim)
  check <- t(apply(testmat, 1, prior$fn, simplify = TRUE))
  if (prior$n_dim == 1L) {
    dim(check) <- c(size, 1)
  }

  try_fetch(
    {
      for (i in nrow(check)) {
        if (!is_double(check[i, ], n = prior$n_dim)) {
          cli::cli_abort(
            "`fn` must always return a double vector of length {prior$n_dim}."
          )
        }
        if (any(!is.finite(check[i, ]))) {
          cli::cli_abort(
            "`fn` must always return finite values for every vector in [0, 1)."
          )
        }
        if (!is_empty(prior$lower) && any(check[i, ] < prior$lower)) {
          cli::cli_abort(c(
            "`fn` must return values greater than or equal to the lower bound.",
            "i" = "Lower bounds: {prior$lower}"
          ))
        }
        if (!is_empty(prior$upper) && any(check[i, ] > prior$upper)) {
          cli::cli_abort(c(
            "`fn` must return values less than or equal to the upper bound.",
            "i" = "Upper bounds: {prior$upper}"
          ))
        }
      }
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "{.cls ernest_prior} failed validation checks.",
          "x" = "Failure at this input: {testmat[i, ]}",
          "x" = "Failed with this output: {check[i, ]}"
        ),
        parent = cnd,
        call = call
      )
    }
  )
  prior
}

#' @export
format.ernest_prior <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_bullets(c(
      "An {.cls ernest_prior}: {x$n_dim} variable{?s}"
    ))
  })
}

#' @export
print.ernest_prior <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
