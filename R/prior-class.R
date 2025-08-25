#' Specify a prior distribution for nested sampling
#'
#' Use an R function to specify the prior distribution of parameters for a
#' nested sampling run.
#'
#' @param fn,rowwise_fn Choose one of `fn` or `rowwise_fn`:
#' * `fn`: A function. Takes a vector of unit cube coordinates and
#'   returns a vector of parameters of the same length.
#' * `rowwise_fn`: A function. Takes a matrix of unit cube coordinates
#'   and returns a matrix of parameters with identical dimensions.
#' @inheritParams create_likelihood
#' @param .n_dim An optional positive integer. The number of dimensions of the
#' prior distribution. If left `NULL`, this is inferred from the common length
#' of the vector-valued parameters (see Note).
#' @param .varnames An optional character vector. Names for the variables in the
#' prior distribution.
#' @param .name_repair An optional, case-sensitive string. How to repair
#' `varnames`. Options are `"unique"` (default), `"universal"`, or
#' `"check_unique"`. See [vctrs::vec_as_names()] for details.
#' @param .lower,.upper Numeric vectors. Expected bounds for the
#' parameter vectors after hypercube transformation.
#'
#' @returns
#' A named list with class `ernest_prior`, containing:
#' * `fn`: The prior transformation function.
#' * `n_dim`: Number of dimensions in the prior space.
#' * `lower`, `upper`: Bounds for the prior distribution,
#' recycled to length `n_dim`.
#'
#' Additionally, the object has a `varnames` attribute.
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
#' transform each value in (0, 1) using the inverse cumulative distribution
#' function (CDF) for each parameter. For more complex cases, you can specify
#' hierarchical or conditionally dependent priors (see Examples).
#'
#' `create_prior` performs regularity checks on your prior function to catch
#' basic errors that may affect nested sampling. To pass these checks, `fn` or
#' `rowwise_fn` must:
#' * Return a finite double vector of length `n_dim` when given
#' `rep(0.5, n_dim)`;
#' * Return a finite double matrix of parameters with the same dimensions, all
#' within the bounds `lower` and `upper` (if provided), when given a random
#' matrix of size `c(5, n_dim)`.
#'
#' Ernest will wrap `fn` so it can accept a matrix of parameters. If you have a
#' more efficient implementation that handles vectors and matrices, consider
#' providing `rowwise_fn` instead.
#'
#' @note The vector-valued parameters in this function are recycled to length
#' `n_dim` if it is an integer, or to a common length if `n_dim = NULL`. See
#' [vctrs::vector_recycling_rules] for additional information on
#' recycling.
#'
#' @srrstats {G2.0a, G2.1a} Documents expectations on the vector parameters
#' `lower` and `upper`.
#' @srrstats {BS1.2, BS1.2c} Specifies how to design a prior in ernest, and
#' provides examples.
#'
#' @aliases ernest_prior
#' @examples
#' # 3D uniform prior in the range [-10, 10]
#' unif <- function(x) {
#'    -10 + x * 20
#' }
#'
#' prior <- create_prior(unif, .n_dim = 3, .lower = -10, .upper = 10)
#' prior$fn(c(0.25, 0.5, 0.75))
#' mat <- matrix(c(0.25, 0.5, 0.75, 0.1, 0.2, 0.3), ncol = 3, byrow = TRUE)
#' prior$fn(mat)
#'
#' # A normal prior with parameterised mean and standard deviation
#' hier_f <- function(theta) {
#'   mu <- qnorm(theta[1], mean = 5) # mu ~ N(5, 1)
#'   sigma <- 10 ^ qunif(theta[2], min = -1, max = 1) # log10(sigma) ~ U[-1, 1]
#'   x <- qnorm(theta[3], mu, sigma) # X ~ N(mu, sigma)
#'   c(mu, sigma, x)
#' }
#' create_prior(
#'   hier_f,
#'   .varnames = c("mu", "sigma", "x"),
#'   .lower = c(-Inf, 0, -Inf)
#' )
#'
#' # Using `rowwise_fn` should be done with care
#' bb_p <- function(x) {
#'   beta <- stats::qbeta(x[1], 5, 5)
#'   bern <- stats::qbinom(x[2], size = 1, beta)
#'   c(beta, bern)
#' }
#' try(
#'   create_prior(
#'     rowwise_fn = bb_p,
#'     .varnames = c("beta", "bern")
#'   )
#' )
#' create_prior(bb_p, .varnames = c("beta", "bern"))
#' @export
create_prior <- function(
  fn,
  rowwise_fn,
  ...,
  .n_dim = NULL,
  .varnames = NULL,
  .name_repair = c("unique", "universal", "check_unique"),
  .lower = -Inf,
  .upper = Inf
) {
  which_arg <- check_exclusive(fn, rowwise_fn)
  .name_repair <- arg_match(.name_repair)
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

  prior <- new_ernest_prior(
    prior_fn = wrap_prior(rowwise_fn),
    n_dim = .n_dim,
    varnames = .varnames,
    lower = .lower,
    upper = .upper
  )

  try_fetch(
    check_prior(prior),
    error = function(cnd) {
      cli::cli_abort(
        "Can't validate `{which_arg}` as a valid prior.",
        parent = cnd
      )
    }
  )
  prior
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
#' @srrstats {G2.0, G2.1} Uses vctrs functions to ensure that the inputs are of
#' the commensurate size and type.
#' @srrstats {G2.4, G2.4a, G2.4b} Explicit conversion of inputs to expected
#' types or error messages for univariate parameters.
#' @srrstats {G2.4c} Ensures that varnames is a unique character string,
#' thanks to the `make.unique()` function.
#' @srrstats {BS2.2, BS2.3} Ensures that the lengths of the prior parameters are
#' validated before the NS algorithm is invoked.
#'
#' @returns `NULL` if all checks pass, otherwise an error is raised.
#' @importFrom cli cli_warn
#' @noRd
check_prior <- function(prior, n_tests = 10, call = caller_env()) {
  result <- prior$fn(rep(0.5, prior$n_dim))
  check_double(
    result,
    size = prior$n_dim,
    arg = glue::glue("prior$fn"),
    call = call
  )

  test <- matrix(stats::runif(n_tests * prior$n_dim), nrow = n_tests)
  result <- prior$fn(test)
  check_matrix(
    result,
    nrow = n_tests,
    ncol = prior$n_dim,
    lower = prior$lower,
    upper = prior$upper,
    arg = glue::glue("prior$fn"),
    call = call
  )
  invisible(NULL)
}

#' Construct an object of class 'ernest_prior'.
#'
#' @param prior_fn The prior transformation function.
#' @param n_dim Number of dimensions.
#' @param varnames Character vector of variable names.
#' @param lower,upper Numeric vectors of bounds.
#' @param class Character vector of additional S3 classes.
#' @param repair Character string indicating how to repair variable names.
#'   Options are `"unique"` (default), `"universal"`, or `"check_unique"`.
#'   See `vctrs::vec_as_names()` for details.
#' @param call The calling environment for error messages.
#'
#' @returns An object of class 'ernest_prior', which is a structured list
#' containing the prior function, dimensionality, variable names, and bounds.
#' @noRd
new_ernest_prior <- function(
  prior_fn,
  n_dim = NULL,
  lower = -Inf,
  upper = Inf,
  varnames = NULL,
  repair = c("unique", "universal", "check_unique"),
  class = NULL,
  call = caller_env()
) {
  check_number_whole(n_dim, min = 1, allow_null = TRUE, call = call)
  c(lower, upper) %<-%
    vctrs::vec_cast_common(
      "lower" = lower,
      "upper" = upper,
      .to = double(),
      .call = caller_env()
    )
  varnames <- vctrs::vec_cast(varnames %||% "", to = character(), call = call)
  c(lower, upper, varnames) %<-%
    vctrs::vec_recycle_common(
      "lower" = lower,
      "upper" = upper,
      "varnames" = varnames,
      .size = n_dim,
      .call = call
    )
  n_dim <- vctrs::vec_size(varnames)
  if (!is.null(repair)) {
    varnames <- vctrs::vec_as_names(
      varnames,
      repair = repair,
      repair_arg = "name_repair"
    )
  }

  if (any(lower >= upper)) {
    error_loc <- as.character(which(lower >= upper))
    cli::cli_abort(
      c(
        "`lower` must be strictly smaller than `upper`.",
        "x" = "Problem at {?index/indicies} {error_loc}."
      ),
      call = call
    )
  }

  structure(
    list(
      "fn" = prior_fn,
      "n_dim" = n_dim,
      "lower" = lower,
      "upper" = upper
    ),
    varnames = varnames,
    class = c(class, "ernest_prior")
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
    cli::cli_text("Prior distribution {.cls {class(x)}}")
    cli::cat_line()
    cli::cli_text("Names: {.val {attr(x, 'varnames')}}")
    if (any(x$lower != -Inf) || any(x$upper != Inf)) {
      cli::cli_bullets(c(
        "Bounds:",
        ">" = "Lower: {x$lower}",
        ">" = "Upper: {x$upper}"
      ))
    }
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
