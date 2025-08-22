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
  prior <- new_ernest_prior(
    prior_fn = \(x) NULL,
    n_dim = n_dim,
    lower = lower,
    upper = upper,
    varnames = varnames,
    repair = name_repair
  )
  dparams <- vctrs::vec_recycle_common(
    "mean" = mean,
    "sd" = sd,
    .size = prior$n_dim
  )

  truncated <- any(is.finite(prior$lower)) || any(is.finite(prior$lower))
  unit <- NULL
  fn_body <- if (truncated) {
    check_installed(
      "truncnorm",
      "To calculate the quantile function of the truncated normal distribution."
    )
    expr({
      y <- truncnorm::qtruncnorm(
        t(unit),
        mean = !!dparams$mean,
        sd = !!dparams$sd,
        a = !!prior$lower,
        b = !!prior$upper
      )
      if (is.null(rows <- nrow(unit))) {
        y
      } else {
        matrix(y, byrow = TRUE, nrow = rows)
      }
    })
  } else {
    expr({
      y <- stats::qnorm(t(unit), mean = !!dparams$mean, sd = !!dparams$sd)
      if (!is.matrix(unit)) {
        dim(y) <- NULL
        y
      } else {
        t(y)
      }
    })
  }

  new_ernest_prior(
    prior_fn = wrap_special_prior(fn_body),
    n_dim = prior$n_dim,
    varnames = attr(prior, "varnames"),
    lower = prior$lower,
    upper = prior$upper,
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
    prior_fn = \(x) NULL,
    n_dim = n_dim,
    lower = lower,
    upper = upper,
    varnames = varnames,
    repair = name_repair
  )

  diff <- prior$upper - prior$lower
  unit <- NULL
  body <- expr({
    if (is.matrix(unit)) {
      y <- sweep(unit, MARGIN = 2, !!diff, `*`)
      sweep(y, MARGIN = 2, !!prior$lower, `+`)
    } else {
      !!prior$lower + (unit * !!diff)
    }
  })

  new_ernest_prior(
    prior_fn = wrap_special_prior(body),
    n_dim = prior$n_dim,
    varnames = attr(prior, "varnames"),
    lower = prior$lower,
    upper = prior$upper,
    class = "uniform_prior"
  )
}

#' Wrap a special prior in type- and size-stability checks.
#'
#' @param body The body of the function accepting a `unit` parameter, as an
#' expression.
#'
#' @returns A type- and size-stable function.
#' @noRd
wrap_special_prior <- function(body) {
  unit <- NULL
  new_function(
    exprs(unit = ),
    expr({
      if (!is.numeric(unit)) {
        stop_input_type(unit, "a numeric vector or matrix")
      }
      !!body
    })
  )
}
