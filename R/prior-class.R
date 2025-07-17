#' Specify a prior distribution for nested sampling
#'
#' Use an R function to specify the prior distribution of parameters for a
#' nested sampling run.
#'
#' @param fn A function that translates coodinates in a unit hypercube into
#' points within the prior parameter space. See details for more information.
#' @param n_dim A whole number, representing the number of dimensions within the
#' prior distribution. Must be larger than or equal to 1.
#' @param varnames A character vector of names for the variables in the
#' prior distribution.
#' @param lower,upper A numeric vector of lower bounds for the prior
#' distribution. Set to `-Inf` and `Inf`, respectively, by default.
#' @param auto_batch Whether the `fn` should be wrapped to allow for matrix
#' inputs. If `FALSE`, it is assumed that `fn` can already handle
#' transforming both vectors and matrices of unit cube coordinates. See details
#' for more information.
#'
#' @returns A named list with class `ernest_prior`. The list contains the
#' following:
#' * `fn`: The prior transformation function.
#' * `n_dim`: The number of dimensions in the prior space.
#' * `varnames`: Names for the variables in the prior distribution, recycled to
#' length `n_dim` and transformed by `make.unique()`.
#' * `lower` and `upper`: Bounds for the prior distribution, recycled to length
#' `n_dim`.
#'
#' @details
#' The unit hypercube transformation encodes points within the parameter space
#' as independent and identically distributed points within a unit hypercube.
#' Implementations of nested sampling, including ernest, use this transformation
#' to simplify likelihood-restricted prior sampling, avoiding uncessary
#' rejection steps.
#'
#' In ernest, the `create_prior` function allows you to specify your own
#' prior distribution. This requires you to provide your own transformation
#' function. In many cases, when the prior can be factorized, this function
#' can simply be transforming a vector of values in (0, 1) and transforming
#' them elementwise with inverse cumulative distribution function (CDF) of
#' each parameter. In more complex cases, you can specify a hierarchical or
#' conditionally-dependent prior (see Examples).
#'
#' ernest will perform several regularity checks on `fn` when the prior is
#' created. These checks are passed if:
#' * `fn` is a function;
#' * `fn(rep(0.5, n_dim))` returns a finite double vector of length `n_dim`;
#' * `fn`, when given a matrix of random unit cube coordinates, returns a
#' finite double matrix with the same dimensions that all respect the specified
#' bounds.
#'
#' As default, `auto_batch` expects that `fn` is incapable of handling matrices
#' of unit cube values. It resolves this by wrapping `fn` in a call to
#' [base::apply()]. Should you have a more efficient implementation of your
#' likelihood function, then consider setting `auto_batch == FALSE`.
#' Failing the checks on `fn` with `auto_batch = FALSE` will remind the user
#' of this setting.
#'
#' @srrstats {BS2.2, BS2.3} This function validates the boundary parameters
#' placed on a user-provided prior, and tests the user's input to ensure
#' the prior accepts and returns vectors and matrices of the expected dimensions
#' as a distinct step before nested sampling.
#'
#' @srrstats {BS1.2, BS1.2c} Describes how to create prior functions for nested
#' sampling.
#'
#' @examples
#' # 3D uniform prior in the range [-10, 10]
#' unif <- function(x) {
#'    -10 + x * 20
#' }
#'
#' prior <- create_prior(unif, n_dim = 3, lower = -10, upper = 10)
#' prior$fn(c(0.25, 0.5, 0.75))
#' mat <- matrix(c(0.25, 0.5, 0.75, 0.1, 0.2, 0.3), ncol = 3, byrow = TRUE)
#' prior$fn(mat)
#'
#' # A normal prior, with a parameterized mean and standard deviation
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
#' # Setting `auto_batch = FALSE` should be done with care
#' bb_p <- function(x) {
#'   beta <- stats::qbeta(x[1], 5, 5)
#'   bern <- stats::qbinom(x[2], size = 1, beta)
#'   c(beta, bern)
#' }
#' try(
#'  create_prior(
#'   bb_p,
#'   n_dim = 2,
#'   varnames = c("beta", "bern"),
#'   auto_batch = FALSE
#'  )
#' )
#' create_prior(bb_p, n_dim = 2, varnames = c("beta", "bern"))
#' @export
create_prior <- function(
  fn,
  n_dim,
  varnames = "X",
  lower = NULL,
  upper = NULL,
  auto_batch = TRUE
) {
  check_bool(auto_batch)
  params <- check_prior_params(n_dim, varnames, lower, upper)

  fn <- rlang::as_function(fn, env = global_env())
  batched_fn <- if (auto_batch) {
    function(x) {
      if (is.matrix(x)) {
        t(apply(x, 1, fn))
      } else {
        fn(x)
      }
    }
  } else {
    fn
  }

  check_prior_fn(
    batched_fn,
    params$n_dim,
    params$lower,
    params$upper,
    auto_batch
  )

  new_ernest_prior(
    batched_fn,
    params$n_dim,
    params$varnames,
    params$lower,
    params$upper
  )
}

#' Validate the parameters for a prior distribution.
#'
#' @param n_dim The number of dimensions for the prior distribution.
#' @param varnames Names for the variables in the prior distribution.
#' @param lower Optional lower bounds for the prior distribution.
#' @param upper Optional upper bounds for the prior distribution.
#'
#' @return Ideally a named list:
#' * A scalar integer `n_dim`.
#' * An `n_dim`-length character vector `varnames` after being recycled
#' and made unique.
#' * An`n_dim`-length double vectors `lower` or `upper`; by default, these
#' contain `-Inf` and `Inf`
#'
#' Alternatively, if checks fail, an error is raised.
#' @importFrom cli cli_abort
#' @noRd
check_prior_params <- function(
  n_dim,
  varnames,
  lower = NULL,
  upper = NULL,
  call = caller_env()
) {
  check_number_whole(n_dim, min = 1, call = call)

  varnames <- vctrs::vec_recycle(
    vctrs::vec_cast(varnames, to = character(), call = call),
    size = n_dim,
    x_arg = "varnames",
    call = call
  )
  varnames <- make.unique(varnames)

  bounds <- vctrs::vec_cast_common(
    "lower" = lower %||% -Inf,
    "upper" = upper %||% Inf,
    .to = double(),
    .call = call
  )
  common_length <- vctrs::vec_recycle_common(
    !!!bounds,
    .size = n_dim,
    .call = call
  )
  if (any(common_length$lower >= common_length$upper)) {
    cli_abort("`lower` must be strictly smaller than `upper`.", call = call)
  }
  c("n_dim" = as.integer(n_dim), "varnames" = list(varnames), common_length)
}

#' Check the validity of a prior transformation function.
#'
#' This function tests that the user-supplied prior transformation function
#' returns finite double vectors/matrices of the correct dimensions and within
#' specified bounds, for both vector and matrix inputs.
#'
#' @param fn The prior transformation function.
#' @param n_dim Number of dimensions for the prior.
#' @param lower,upper Numeric vectors of lower and upper bounds.
#' @param auto_batch Logical; whether batching is enabled.
#' @param call Environment for error reporting.
#'
#' @returns `NULL` if all checks pass, otherwise an error is raised.
#' @importFrom rlang try_fetch is_double
#' @importFrom cli cli_warn
#' @noRd
check_prior_fn <- function(
  fn,
  n_dim,
  lower,
  upper,
  auto_batch = TRUE,
  call = caller_env()
) {
  check_function(fn, call = call)

  try_fetch(
    {
      result <- fn(rep(0.5, n_dim))
      if (!rlang::is_double(result)) {
        cli_abort(c(
          "`fn` must return a double vector.",
          "x" = "`fn` instead returned a {obj_type_friendly(result)}"
        ))
      }
      if (length(result) != n_dim) {
        cli_abort(c(
          "`fn` must return a vector of length {n_dim}.",
          "x" = "`fn` instead returned a vector of length {length(result)}"
        ))
      }
      if (any(!is.finite(result))) {
        cli_abort("`fn` must always return finite values.")
      }
    },
    error = function(cnd) {
      cli_abort(
        "Can't transform the point at `rep(0.5, {n_dim}).",
        parent = cnd,
        call = call
      )
    }
  )

  if (1000 %/% n_dim < 1L) {
    cli_warn("Skipping matrix testing, as `n_dim` > 1000.")
  }
  test <- matrix(runif(1000 %/% n_dim), ncol = n_dim)
  try_fetch(
    {
      result <- fn(test)
      if (!is.matrix(test) || !is.double(test)) {
        cli_abort(c(
          "`fn` must return a double matrix.",
          "x" = "`fn` instead returned a {obj_type_friendly(result)}",
        ))
      }
      if (!identical(dim(test), dim(result))) {
        cli_abort(c(
          "`fn` must preserve the dimensions of an input matrix.",
          "x" = "`fn` returned a matrix of dimensionality {dim(result)}"
        ))
      }
      if (any(!is.finite(test))) {
        cli_abort("`fn` must always return finite values.")
      }
      if (any(sweep(result, 2, lower, `<`))) {
        cli_abort(
          "`fn` must return values greater than or equal to `lower`."
        )
      }
      if (any(sweep(result, 2, upper, `>`))) {
        cli_abort(
          "`fn` must return values lesser than or equal to `upper`."
        )
      }
    },
    error = function(cnd) {
      cli_abort(
        c(
          "Can't transform a test matrix of dim. {dim(test)}.",
          "i" = if (!auto_batch) {
            "Can `fn` handle matrices without setting `auto_batch = TRUE`?"
          } else {
            NULL
          }
        ),
        parent = cnd,
        call = call
      )
    }
  )
  NULL
}

#' Construct an object of class 'ernest_prior'.
#'
#' @param prior_fn The prior transformation function.
#' @param n_dim Number of dimensions.
#' @param varnames Variable names.
#' @param lower,upper Numeric vectors of bounds.
#' @param class Additional class attributes.
#'
#' @returns An object of class 'ernest_prior', which is a structured list
#' containing the prior function, dimensionality, variable names, and bounds.
#' @noRd
new_ernest_prior <- function(
  prior_fn,
  n_dim,
  varnames = NULL,
  lower = NULL,
  upper = NULL,
  class = NULL
) {
  structure(
    list(
      "fn" = prior_fn,
      "n_dim" = n_dim,
      "varnames" = varnames,
      "lower" = lower,
      "upper" = upper
    ),
    class = c("ernest_prior", class)
  )
}

#' Format for ernest_prior
#'
#' @param x An object of class 'ernest_prior'.
#' @param Ignored.
#'
#' @returns A formatted string describing the prior object.
#' @noRd
#' @export
format.ernest_prior <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_bullets(c(
      "An {.cls {class(x)}}: {x$n_dim} variable{?s}"
    ))
  })
}

#' Print for ernest_prior
#'
#' @param x An object of class 'ernest_prior'.
#' @param Ignored.
#'
#' @returns `x`, invisibly.
#' @noRd
#' @export
print.ernest_prior <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
