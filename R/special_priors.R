#' Specify a prior with normally distributed marginals
#'
#' A specialisation of [create_prior()] where the parameter space is
#' described by independent normal variables, possibly truncated.
#'
#' @inheritParams stats::qnorm
#' @param mean Numeric vector of means.
#' @param sd Numeric vector of standard deviations (must be strictly positive.)
#' @param lower,upper Numeric vector of bounds for a truncated normal
#' distribution.
#' @inheritParams create_prior
#'
#' @returns A `normal_prior`, a subclass of `ernest_prior` with an efficient
#' implementation of the unit hypercube transformation.
#'
#' @inherit create_prior note
#'
#' @srrstats {BS2.5} create_normal_prior performs checks to ensure that the sd
#' parameter is strictly positive.
#'
#' @seealso
#' * [create_prior()] for more on the `ernest_prior` object.
#' * [truncnorm::qtruncnorm()] for the truncated normal quantile function.
#' @family special_priors
#' @export
#' @examples
#' prior <- create_normal_prior(n_dim = 3)
#' prior$fn(c(0.25, 0.5, 0.75))
create_normal_prior <- function(
  n_dim = NULL,
  mean = 0,
  sd = 1,
  lower = -Inf,
  upper = Inf,
  varnames = "Normal",
  name_repair = c("unique", "universal", "check_unique")
) {
  mean <- vctrs::vec_cast(mean, double())
  sd <- vctrs::vec_cast(sd, double())
  if (any(sd <= 0)) {
    cli::cli_abort("`sd` of a normal distribution must be non-negative.")
  }

  params <- vctrs::vec_recycle_common(
    mean = mean,
    sd = sd,
    lower = lower,
    upper = upper,
    varnames = varnames,
    .size = n_dim
  )
  n_dim <- n_dim %||% vctrs::vec_size(params$lower)

  bounds <- vctrs::vec_cast_common(
    lower = params$lower,
    upper = params$upper,
    .to = double()
  )

  truncated <- any(is.finite(bounds$lower)) || any(is.finite(bounds$upper))
  x <- NULL
  body <- if (truncated) {
    check_installed(
      "truncnorm",
      "To calculate the quantile function of the truncated normal distribution."
    )
    expr(
      (!!truncnorm::qtruncnorm)(
        x,
        mean = !!params$mean,
        sd = !!params$sd,
        a = !!bounds$lower,
        b = !!bounds$upper
      )
    )
  } else {
    expr((!!stats::qnorm)(x, mean = !!params$mean, sd = !!params$sd))
  }

  fn <- new_function(
    exprs(x = ),
    expr(!!body),
    env = empty_env()
  )

  new_ernest_prior(
    fn = fn,
    n_dim = n_dim,
    varnames = params$varnames,
    lower = params$lower,
    upper = params$upper,
    repair = name_repair,
    class = "normal_prior"
  )
}

#' Specify a prior with uniformly distributed marginals
#'
#' A specialisation of [create_prior()] where the parameter space is
#' described by independent uniform marginals.
#'
#' @param lower,upper Numeric vector of bounds for the uniform distribution.
#' @inheritParams create_prior
#'
#' @returns A `uniform_prior`, a subclass of `ernest_prior` with an efficient
#' implementation of the unit hypercube transformation.
#'
#' @inherit create_prior note
#'
#' @family special_priors
#' @export
#' @examples
#' prior <- create_uniform_prior(lower = c(3, -2), upper = c(5, 4))
#' prior$fn(c(0.33, 0.67))
create_uniform_prior <- function(
  n_dim = NULL,
  lower = 0,
  upper = 1,
  varnames = "Uniform",
  name_repair = c("unique", "universal", "check_unique")
) {
  prior <- new_ernest_prior(
    fn = \(x) NULL,
    n_dim = n_dim,
    lower = lower,
    upper = upper,
    varnames = varnames,
    repair = name_repair
  )

  diff <- prior$upper - prior$lower
  x <- NULL
  fn <- new_function(
    exprs(x = ),
    expr(!!prior$lower + (x * !!diff))
  )

  new_ernest_prior(
    fn = fn,
    n_dim = prior$n_dim,
    varnames = attr(prior, "varnames"),
    lower = prior$lower,
    upper = prior$upper,
    class = "uniform_prior"
  )
}
