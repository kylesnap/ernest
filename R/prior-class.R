#' Specify a prior distribution for nested sampling
#'
#' Use an R function to specify the prior distribution of parameters for a
#' nested sampling run.
#'
#' @param fn,rowwise_fn (function) Pick one of `fn` or `rowwise_fn`:
#' * `fn`: A function that takes in a vector of unit cube coordinates and
#' returns a same-size vector of parameters.
#' * `rowwise_fn`: A function that takes in a vector of unit cube coordinates
#' and returns a matrix of parameters with identical dimensions.
#' @param n_dim (positive integer) The dimensionality of the prior distribution.
#' @param varnames (optional character vector) A character vector of names for
#' the variables in the prior distribution.
#' @param lower,upper (optional numeric vector) The expected bounds for the
#' parameter vectors after hypercube transformation. Set to `-Inf` and `Inf`
#' by default.
#' @inheritParams create_likelihood
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
#' `create_prior` allows you to specify your own prior distribution.
#' This requires you to provide your own transformation
#' function. In many cases, when the prior can be factorized, this function
#' can simply be transforming a vector of values in (0, 1) and transforming
#' them elementwise with inverse cumulative distribution function (CDF) of
#' each parameter. In more complex cases, you can specify a hierarchical or
#' conditionally-dependent prior (see Examples).
#'
#' `create_prior` will perform several regularity checks on your prior function.
#' This intends to capture basic errors that may confound ernest's nested
#' sampling implementation. To pass these checks, `fn` or `rowwise_fn` must:
#' * return a finite double vector of length `n_dim` when provided
#' `rep(0.5, n_dim)`,
#' * return a finite double matrix of parameters with the same dimensions that
#' all respect the bounds `lower` and `upper` (if provided) when provided
#' random matrix with dimensions `c(5, n_dim)`.
#'
#' Ernest will wrap `fn` so that it may take in a matrix of parameters. Should
#' you have a more efficient implementation of your likelihood function that
#' can handle vectors and matrices, then consider providing `rowwise_fn`
#' instead.
#'
#' @srrstats {BS2.2, BS2.3} This function validates the boundary parameters
#' placed on a user-provided prior, and tests the user's input to ensure
#' the prior accepts and returns vectors and matrices of the expected dimensions
#' as a distinct step before nested sampling.
#'
#' @srrstats {BS1.2, BS1.2c} Describes how to create prior functions for nested
#' sampling.
#'
#' @aliases ernest_prior
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
  rowwise_fn,
  n_dim,
  ...,
  varnames = "X",
  lower = -Inf,
  upper = Inf
) {
  which_arg <- check_exclusive(fn, rowwise_fn)
  rowwise_fn <- if (which_arg == "fn") {
    fn <- as_function(fn, arg = "fn")
    if (dots_n(...) != 0L) {
      fn <- purrr::partial(fn, ...)
    }
    attr(fn, "is_rowwise") <- FALSE
    as_rowwise_fn(fn)
  } else {
    rowwise_fn <- as_function(rowwise_fn, arg = "rowwise_fn")
    if (dots_n(...) != 0L) {
      rowwise_fn <- purrr::partial(rowwise_fn, ...)
    }
    rowwise_fn
  }
  params <- check_prior_params(n_dim, varnames, lower, upper)

  # fn,
  # n_dim,
  # lower,
  # upper,
  # n_tests = 10,
  # call = caller_env()
  prior_fn <- wrap_prior(rowwise_fn)
  try_fetch(
    check_prior_fn(
      prior_fn,
      params$n_dim,
      params$lower,
      params$upper,
      call = caller_env()
    ),
    error = function(cnd) {
      cli::cli_abort(
        "Can't validate `{which_arg}` as a valid prior.",
        parent = cnd
      )
    }
  )

  new_ernest_prior(
    prior_fn,
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
  lower,
  upper,
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
    "lower" = lower,
    "upper" = upper,
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
  list2(
    "n_dim" = as.integer(n_dim),
    "varnames" = c(varnames),
    !!!common_length
  )
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
#' @importFrom cli cli_warn
#' @noRd
check_prior_fn <- function(
  fn,
  n_dim,
  lower,
  upper,
  n_tests = 10,
  call = caller_env()
) {
  result <- fn(rep(0.5, n_dim))
  check_double(
    result,
    size = n_dim,
    arg = glue::glue("prior(rep(0.5, {n_dim}))"),
    call = call
  )

  test <- matrix(stats::runif(n_tests * n_dim), nrow = n_tests)
  result <- fn(test)
  check_matrix(
    result,
    nrow = n_tests,
    ncol = n_dim,
    lower = lower,
    upper = upper,
    arg = glue::glue("prior(matrix(nrow = {n_tests}, ncol = {n_dim}))"),
    call = call
  )
  invisible(NULL)
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

#' Wrap a prior in catchers to promote its size- and type-stability
#' @param rowwise_fn A prior function that can operate on matrices and vectors.
#' @returns A function that outputs matrices and vectors of the expected type
#' (double) and size, or errors.
#' @noRd
wrap_prior <- function(rowwise_fn) {
  unit <- NULL
  new_function(
    exprs(unit = ),
    expr({
      if (!is.numeric(unit)) {
        stop_input_type(unit, "a numeric vector or matrix")
      }
      y <- (!!rowwise_fn)(unit)
      if (!is.double(y)) {
        cli::cli_abort(c(
          "`prior(unit)` must always return a vector or matrix of doubles.",
          "x" = "Instead, it returned {obj_type_friendly(y)}."
        ))
      }
      if (any(is.na(y)) || any(is.nan(y))) {
        cli::cli_abort("`prior(unit)` must never return `NA` or `NaN` values.")
      }
      if (is.matrix(unit) && !isTRUE(all.equal(dim(unit), dim(y)))) {
        cli::cli_abort(c(
          "`prior(unit)` must return a matrix of equal dim. to `unit`.",
          "x" = "Expected dim(y) = {nrow(unit)} x {ncol(unit)}.",
          "x" = "Returned dim(y) = {dim(y)}."
        ))
      } else {
        vctrs::vec_check_size(y, vctrs::vec_size(unit))
      }
      y
    })
  )
}
