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
#' prior <- create_normal_prior(.n_dim = 3)
#' prior$fn(c(0.25, 0.5, 0.75))
create_normal_prior <- function(
  mean = 0,
  sd = 1,
  names = "Normal",
  lower = -Inf,
  upper = Inf,
  .n_dim = NULL,
  .name_repair = c("unique", "universal", "check_unique")
) {
  mean <- vctrs::vec_cast(mean, double())
  sd <- vctrs::vec_cast(sd, double())
  if (any(sd <= 0)) {
    cli::cli_abort(
      "All elements of {.arg sd} must be strictly positive and non-missing."
    )
  }
  n_dim <- vctrs::vec_size_common(
    mean = mean,
    sd = sd,
    lower = lower,
    upper = upper,
    names = names,
    .size = .n_dim
  )

  prior <- new_ernest_prior(
    fn = \(x) x,
    n_dim = n_dim,
    lower = lower,
    upper = upper,
    names = names,
    name_repair = .name_repair,
    class = "uniform_prior"
  )
  params <- vctrs::vec_recycle_common(
    mean = mean,
    sd = sd,
    .size = n_dim
  )

  truncated <- any(is.finite(prior$lower)) || any(is.finite(prior$upper))
  unit <- NULL
  prior$fn <- if (truncated) {
    check_installed(
      "truncnorm",
      "calculate the quantile function of the truncated normal distribution"
    )
    new_function(
      exprs(unit = ),
      expr((!!truncnorm::qtruncnorm)(
        unit,
        mean = !!params$mean,
        sd = !!params$sd,
        a = !!prior$lower,
        b = !!prior$upper
      )),
      env = empty_env()
    )
  } else {
    new_function(
      exprs(unit = ),
      expr((!!stats::qnorm)(unit, mean = !!params$mean, sd = !!params$sd)),
      env = empty_env()
    )
  }

  do.call(
    new_ernest_prior,
    c(
      as.list(prior),
      "mean" = params$mean,
      "sd" = params$sd,
      class = "normal_prior"
    )
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
  lower = 0,
  upper = 1,
  names = "Uniform",
  .n_dim = NULL,
  .name_repair = c("unique", "universal", "check_unique")
) {
  prior <- new_ernest_prior(
    fn = \(x) x,
    n_dim = .n_dim,
    lower = lower,
    upper = upper,
    names = names,
    class = "uniform_prior"
  )
  diff <- prior$upper - prior$lower
  prior$fn <- function(x) {
    prior$lower + x * diff
  }

  do.call(
    new_ernest_prior,
    c(as.list(prior), class = "uniform_prior")
  )
}
