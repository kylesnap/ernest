#' Specify a prior with normally distributed marginals
#'
#' A specialisation of [create_prior()] where the parameter space is
#' described by independent normal variables, possibly truncated.
#'
#' @inheritParams create_prior
#' @inheritParams stats::qnorm
#' @param lower,upper Double vectors. Bounds for a truncated distribution.
#' Recycled to length `n_dim`.
#'
#' @returns A `normal_prior`, a subclass of `ernest_prior` with an efficient
#' implementation of the unit hypercube transformation.
#'
#' @seealso
#' * [create_prior()] for more on the `ernest_prior` object.
#' * [truncnorm::qtruncnorm()] for the truncated normal quantile function.
#' @family special_priors
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
  mean <- vctrs::vec_cast(mean, double())
  sd <- vctrs::vec_cast(sd, double())
  if (any(sd <= 0)) {
    cli::cli_abort("`sd` of a normal distribution must be non-negative.")
  }

  params <- check_prior_params(n_dim, varnames %||% "", lower, upper)
  dparams <- vctrs::vec_recycle_common(
    "mean" = mean,
    "sd" = sd,
    .size = params$n_dim
  )
  if (is.null(varnames)) {
    new_names <- sprintf(
      "N(%s, %s)",
      round(dparams$mean, 3),
      round(dparams$sd^2, 3)
    )
    params$varnames <- make.unique(new_names)
  }

  truncated <- any(is.finite(params$lower)) || any(is.finite(params$lower))
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
        a = !!params$lower,
        b = !!params$upper
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
    n_dim = params$n_dim,
    varnames = params$varnames,
    lower = params$lower,
    upper = params$upper,
    class = "normal_prior"
  )
}

#' Specify a prior with uniformly distributed marginals
#'
#' A specialisation of [create_prior()] where the parameter space is
#' described by independent uniform marginals.
#'
#' @inheritParams create_normal_prior
#' @param lower,upper Double vectors. Bounds of the distribution.
#'
#' @returns A `uniform_prior`, a subclass of `ernest_prior` with an efficient
#' implementation of the unit hypercube transformation.
#'
#' @family special_priors
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
  params <- check_prior_params(n_dim, varnames %||% "", lower, upper)
  if (any(is.infinite(lower)) || any(is.infinite(upper))) {
    cli::cli_abort("Lower and upper must be vectors of finite doubles.")
  }
  if (is.null(varnames)) {
    new_names <- sprintf(
      "U[%s, %s]",
      round(params$lower, 3),
      round(params$upper, 3)
    )
    params$varnames <- make.unique(new_names)
  }

  diff <- params$upper - params$lower
  unit <- NULL
  body <- expr({
    if (is.matrix(unit)) {
      y <- sweep(unit, MARGIN = 2, !!diff, `*`)
      sweep(y, MARGIN = 2, !!params$lower, `+`)
    } else {
      !!params$lower + (unit * !!diff)
    }
  })

  new_ernest_prior(
    prior_fn = wrap_special_prior(body),
    n_dim = params$n_dim,
    varnames = params$varnames,
    lower = params$lower,
    upper = params$upper,
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
