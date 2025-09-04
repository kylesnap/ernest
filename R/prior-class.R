#' Specify a prior distribution for nested sampling
#'
#' Use an R function to specify the prior distribution of parameters for a
#' nested sampling run.
#'
#' @param fn A function. Takes a vector of unit cube coordinates and
#' returns a vector of parameters of the same length.
#' @param rowwise_fn `r lifecycle::badge('deprecated')` No longer supported;
#' instead, use `fn` to provide a prior-transformation function.
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
#' * `lower`, `upper`: Bounds for the prior distribution, recycled to length
#' `n_dim`.
#'
#' Additionally, the object has a `varnames` attribute.
#'
#' When provided a vector from the unit hypercube, `fn` either a length-`n_dim`
#' vector of doubles, containing transformed model parameters, or an error
#' message.
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
#' basic errors that may affect nested sampling. To pass these checks, `fn` must
#' be able to take in a vector of points (each between 0 and 1) and return
#' a vector of the same length which contains only finite values.
#'
#' @note The vector-valued parameters in this function are recycled to length
#' `.n_dim` if it is an integer, or to a common length if `.n_dim = NULL`. See
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
#' @export
create_prior <- function(
  fn,
  rowwise_fn = deprecated(),
  ...,
  .n_dim = NULL,
  .varnames = NULL,
  .name_repair = c("unique", "universal", "check_unique"),
  .lower = -Inf,
  .upper = Inf
) {
  if (lifecycle::is_present(rowwise_fn)) {
    lifecycle::deprecate_warn(
      "1.1.0",
      "ernest::create_prior(rowwise_fn =)",
      "ernest::create_prior(fn =)"
    )
    fn <- rowwise_fn
  }
  fn <- as_function(fn)
  if (dots_n(...) > 0L) {
    fn <- purrr::partial(fn, ...)
  }
  .name_repair <- arg_match(.name_repair)

  prior <- new_ernest_prior(
    fn = fn,
    n_dim = .n_dim,
    varnames = .varnames,
    lower = .lower,
    upper = .upper
  )

  try_fetch(
    check_prior(prior),
    error = function(cnd) {
      cli::cli_abort(
        "Can't validate `fn` as a valid prior.",
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
  result <- apply(test, 1, prior$fn) |> t()
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
#' @param fn The prior transformation function.
#' @param ... Arguments to be forwarded to `fn`.
#' @param n_dim Number of dimensions.
#' @param varnames Character vector of variable names.
#' @param lower,upper Numeric vectors of bounds.
#' @param class Character vector of additional S3 classes.
#' @param repair Character string indicating how to repair variable names.
#'   Options are `"unique"` (default), `"universal"`, or `"check_unique"`.
#' @param call The calling environment for error messages.
#'
#' @returns An object of class 'ernest_prior', which is a structured list
#' containing the prior function, dimensionality, variable names, and bounds.
#' @noRd
new_ernest_prior <- function(
  fn = NULL,
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
    error_loc <- which(lower >= upper)
    cli::cli_abort(
      c(
        "`lower` must be strictly smaller than `upper`.",
        "x" = "Problem at {cli::qty(length(error_loc))} location{?s} {error_loc}:",
        ">" = "`$lower`: {lower[error_loc]}.",
        ">" = "`$upper`: {upper[error_loc]}."
      ),
      call = call
    )
  }

  x <- NULL
  safely <- if (is.null(fn)) {
    NULL
  } else {
    function(x) {
      y <- fn(x)
      y <- vctrs::vec_cast(y, double())
      vctrs::vec_check_size(y, size = n_dim, arg = "prior$fn(x)")
      if (any(!is.finite(y))) {
        unique_nonfinite <- unique(y[!is.finite(y)])
        cli::cli_abort("Priors must only contain finite values, not {y}.")
      }
      y
    }
  }

  structure(
    list(
      "fn" = safely,
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
