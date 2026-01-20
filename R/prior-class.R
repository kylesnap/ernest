#' Specify a prior distribution for nested sampling
#'
#' Use an R function to specify the prior distribution of parameters for a
#' nested sampling run.
#'
#' @param fn A function specifying the prior transformation.
#' @param names Unique names for each variable in the prior distribution.
#' Optional for non-custom prior distributions.
#' @param lower,upper Numeric vectors. Expected bounds for the
#' parameter vectors after hypercube transformation.
#' @param repair Describes how to repair the vector of `names`. One of
#' `"check_unique"`, `"unique"`, `"universal"`, `"unique_quiet"`,
#' or `"universal_quiet"`. See [vctrs::vec_as_names()] for descriptions of each
#' repair strategy.
#'
#' @returns
#' A list with class `ernest_prior`, containing `fn`, `lower`, `upper`,
#' and `names`. The vector-valued parameters are guaranteed to be of common
#' length.
#'
#' @details
#' The unit hypercube transformation encodes points in the parameter space
#' as independent and identically distributed points within a unit hypercube.
#' Nested sampling implementations, including ernest, use this transformation
#' to simplify likelihood-restricted prior sampling and avoid unnecessary
#' rejection steps.
#'
#' `create_prior` allows you to specify your own prior distribution by providing
#' a transformation function. For factorisable priors, this function can simply
#' transform each value in the closed interval (0, 1) using the inverse
#' cumulative distribution function (CDF) for each parameter. For more complex
#' cases, you can specify hierarchical or conditionally dependent priors
#' (see Examples).
#'
#' `create_prior` performs regularity checks on your prior function to catch
#' basic errors that may affect nested sampling. To pass these checks, the
#' function must take in a vector of points (each between 0 and 1)
#' and return a vector or matrix of the same shape containing only finite
#' values.
#'
#' @note See [vctrs::vector_recycling_rules] for additional information on
#' how parameters are recycled to a common length.
#'
#' @srrstats {G2.0a, G2.1a} Documents expectations on the vector parameters
#' `lower` and `upper`.
#' @srrstats {BS1.2, BS1.2c} Specifies how to design a prior in ernest, and
#' provides examples.
#'
#' @aliases ernest_prior
#' @rdname ernest_prior
#' @example ./data-raw/EXAMPLE_PRIOR_CLASS.R
#' @export
create_prior <- function(
  fn,
  names = NULL,
  lower = NULL,
  upper = NULL,
  repair = c(
    "unique",
    "universal",
    "check_unique",
    "unique_quiet",
    "universal_quiet"
  )
) {
  # Check that exactly one of fn or matrix_fn is provided
  fn <- as_function(fn)
  new_ernest_prior(
    fn,
    names,
    lower,
    upper,
    class = "custom_prior",
    repair = repair
  )
}

#' Construct an ernest_prior object
#' @noRd
new_ernest_prior <- function(
  fn,
  names,
  lower = NULL,
  upper = NULL,
  class = NULL,
  repair = "check_unique"
) {
  names <- vctrs::vec_as_names(names, repair = repair)
  n_dim <- length(names)
  if (n_dim < 1) {
    cli::cli_abort("`names` must be at least length one, not length {n_dim}.")
  }

  force(fn)
  catching_fn <- function(x) {
    out <- fn(x)
    out <- vctrs::vec_cast(out, double(n_dim), call = NULL)
    if (any(!is.finite(out))) {
      cli::cli_abort(
        "`fn` cannot return non-numeric, missing, or `NaN` values.",
        call = NULL
      )
    }
    out
  }

  rowwise_fn <- function(x) {
    if (!is_double(x)) {
      stop_input_type(x, "a numeric vector or matrix", call = NULL)
    } else if (!is.matrix(x)) {
      catching_fn(x)
    } else {
      y <- t(apply(x, 1, catching_fn))
      if (n_dim == 1) {
        dim(y) <- c(length(y), 1)
      }
      y
    }
  }
  test_matrix <- check_prior(rowwise_fn, n_dim)

  lower <- lower %||% fn(rep(0, n_dim))
  upper <- upper %||% fn(rep(1, n_dim))
  bounds <- vctrs::vec_cast_common(
    "lower" = lower,
    "upper" = upper,
    .to = double()
  )
  bounds <- vctrs::vec_recycle_common(!!!bounds, .size = n_dim)
  if (any(bounds$lower >= bounds$upper)) {
    cli::cli_abort(
      "`lower` bounds must be strictly smaller than `upper`.",
    )
  }

  check_matrix(
    test_matrix,
    1000,
    n_dim,
    lower = bounds$lower,
    upper = bounds$upper,
    call = call
  )
  rm(test_matrix)

  structure(
    list(
      "fn" = rowwise_fn,
      "names" = names,
      "lower" = bounds$lower,
      "upper" = bounds$upper
    ),
    n_dim = n_dim,
    body = expr(!!fn),
    class = c(class, "ernest_prior")
  )
}

#' @rdname ernest_prior
#'
#' @param x,y `ernest_prior` objects.
#'
#' @export
`+.ernest_prior` <- function(x, y) {
  check_class(x, "ernest_prior")
  check_class(y, "ernest_prior")

  n_dim <- c(attr(x, "n_dim"), attr(y, "n_dim"))
  bodies <- list(attr(x, "body"), attr(y, "body"))
  cum_dim <- cumsum(n_dim)

  fn <- new_function(
    exprs(x = ),
    expr({
      vctrs::vec_c(
        (!!bodies[[1]])(x[1:!!cum_dim[1]]),
        (!!bodies[[2]])(x[!!(cum_dim[1] + 1):!!cum_dim[2]])
      )
    })
  )

  new_ernest_prior(
    fn = fn,
    names = c(x$names, y$names),
    lower = c(x$lower, y$lower),
    upper = c(x$upper, y$upper),
    repair = "unique",
    class = "composite_prior"
  )
}

#' Check the validity of a prior transformation function.
#'
#' This function tests that the user-supplied prior transformation function
#' returns finite double vectors/matrices of the correct dimensions and within
#' specified bounds, for both vector and matrix inputs.
#'
#' @param fn The prior transformation function.
#' @param n_dim Dimensionality.
#' @param lower,upper Numeric vectors. Expected bounds for the
#' parameter vectors after hypercube transformation.
#' @param arg Argument name for error reporting.
#' @param call Environment for error reporting.
#'
#' @srrstats {G2.0, G2.1} Uses vctrs functions to ensure that the inputs are of
#' the commensurate size and type.
#' @srrstats {G2.4, G2.4a, G2.4b} Explicit conversion of inputs to expected
#' types or error messages for univariate parameters.
#' @srrstats {G2.4c} Ensures that names is a unique character string,
#' thanks to the `make.unique()` function.
#' @srrstats {BS2.2, BS2.3} Ensures that the lengths of the prior parameters are
#' validated before the NS algorithm is invoked.
#'
#' @returns The test matrix if all checks pass.
#' @importFrom cli cli_warn
#' @noRd
check_prior <- function(
  fn,
  n_dim,
  lower = -Inf,
  upper = Inf,
  arg = caller_arg(fn),
  call = caller_env()
) {
  test_vector <- rep(0.5, n_dim)
  output_test <- fn(test_vector)
  output_test <- vctrs::vec_cast(
    output_test,
    matrix(double(), ncol = n_dim),
    x_arg = arg,
    call = call
  )

  test_matrix <- matrix(stats::runif(1000 * n_dim), nrow = 1000)
  output_test_mat <- fn(test_matrix)
  check_matrix(
    output_test_mat,
    nrow = 1000,
    ncol = n_dim,
    arg = arg,
    call = call
  )
  output_test_mat
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
  name <- sub("_prior", "", class(x)[[1]])
  cli::format_inline(
    "{name} prior distribution with {attr(x, 'n_dim')} dimensions ({x$names})"
  )
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
