#' Specify a Prior Distribution for Nested Sampling
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
#' * `fn`: The function that computes the quantiles.
#' * `n_dim`: The number of dimensions for the prior distribution.
#' * `varnames`: Names for the variables in the prior distribution.
#' * `lower`: Lower bounds for the prior distribution.
#' * `upper`: Upper bounds for the prior distribution.
#' @export
create_prior <- function(fn, n_dim, varnames = NULL, lower = NULL, upper = NULL) {
  quantile <- call2(fn, expr(p))
  prior_fn <- new_function(
    exprs(p = ),
    expr({
      if (!is_double(p, n = !!n_dim)) {
        cli::cli_abort(c(
          "`p` must be a double vector of length {!!n_dim}.",
        ))
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

#' Special Prior Distributions
#'
#' These functions create specific types of prior distributions for use in
#' ernest.
#'
#' @param mean Vector of means.
#' @param sd Vector of standard deviations.
#' @inheritParams create_prior
#'
#' @returns A subclass of `ernest_prior`.
#' @rdname create_special_prior
#' @export
create_normal_prior <- function(n_dim, mean, sd, varnames = NULL, lower = -Inf, upper = Inf) {
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

#' @param df,mu,sigma Vector of parameters for the Student-T distribution.
#' @param ncp Optional vector of non-centrality parameters.
#'
#' @rdname create_special_prior
#' @export
create_t_prior <- function(n_dim, df, mu = 0, sigma = 1, ncp = NULL, varnames = NULL, lower = -Inf, upper = Inf) {
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

  prior <- new_function(
    exprs(p = ),
    expr({
      if (!is_double(p, n = !!n_dim)) {
        cli::cli_abort(c(
          "`p` must be a double vector of length {!!n_dim}.",
        ))
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

#' @param location,scale Vector of parameters for the Cauchy distribution.
#'
#' @rdname create_special_prior
#' @export
create_cauchy_prior <- function(n_dim, location = 0, scale = 1, varnames = NULL, lower = -Inf, upper = Inf) {
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

  prior <- new_function(
    exprs(p = ),
    expr({
      if (!is_double(p, n = !!n_dim)) {
        cli::cli_abort(c(
          "`p` must be a double vector of length {!!n_dim}.",
        ))
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
    varnames <- sprintf("Uniform(%s, %s)", round(args$lower, 3), round(args$upper, 3))
  }

  quantile <- expr(
    stats::qunif(p = p, min = !!args$lower, max = !!args$upper)
  )

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
new_ernest_prior <- function(prior_fn, n_dim, varnames = NULL, lower = NULL, upper = NULL, class = NULL) {
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
  validate_prior(obj)
}

#' Validation for a prior function
#' @returns obj if valid, else an error
#' @noRd
validate_prior <- function(prior, size = 1000L) {
  check_number_whole(size, min = 1)
  test_val <- prior$fn(rep(0.5, prior$n_dim))
  if (!is_double(test_val, n = prior$n_dim)) {
    cli::cli_abort(c(
      "Prior function must return a double vector of length {prior$n_dim}.",
      "i" = "Returned: {rep(0.5, prior$n_dim)} = {test_val}"
    ))
  }

  testmat <- matrix(runif(size * prior$n_dim), ncol = prior$n_dim)
  check <- t(apply(testmat, 1, prior$fn, simplify = TRUE))
  if (prior$n_dim == 1L) {
    dim(check) <- c(size, 1)
  }

  if (any(!is.finite(check))) {
    indx <- which(!is.finite(check), arr.ind = TRUE)[1,]
    cli::cli_abort(c(
      "Prior must return finite values for all inputs in [0, 1).",
      "i" = "Failed first on row: {testmat[indx]} = {check[indx]}"
    ))
  }
  if (!is_empty(prior$lower)) {
    cmp <- matrix(rep(prior$lower, times = size), nrow = size, byrow = TRUE)
    if (any(check < cmp)) {
      indx <- which(check < cmp, arr.ind = TRUE)[1,]
      cli::cli_abort(c(
        "Prior must return values greater than or equal to the lower bound.",
        "i" = "Failed first on row: {testmat[indx]} = {check[indx]}"
      ))
    }
  }
  if (!is_empty(prior$upper)) {
    cmp <- matrix(rep(prior$upper, times = size), nrow = size, byrow = TRUE)
    if (any(check > cmp)) {
      indx <- which(check > cmp, arr.ind = TRUE)[1,]
      cli::cli_abort(c(
        "Prior must return values less than or equal to the upper bound.",
        "i" = "Failed first on row: {testmat[indx]} = {check[indx]}"
      ))
    }
  }
  prior
}
