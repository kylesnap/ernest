#' Specify a prior distribution for nested sampling
#'
#' Use an R function to specify the prior distribution of parameters for a
#' nested sampling run.
#'
#' @param fn A function. Takes a vector of unit cube coordinates and
#' returns a vector of parameters of the same length.
#' @param names An optional character vector. Names for the variables in the
#' prior distribution.
#' @param lower,upper Numeric vectors. Expected bounds for the
#' parameter vectors after hypercube transformation.
#' @param .n_dim An optional positive integer. The number of dimensions of the
#' prior distribution.
#' @param .name_repair An optional, case-sensitive string. How to repair
#' `names`. Options are `"unique"` (default), `"universal"`, or
#' `"check_unique"`. See [vctrs::vec_as_names()] for details.
#'
#' @returns
#' A list with with class `ernest_prior`, containing `fn`, `lower`, `upper`,
#' and `names`. The vector-valued parameters are guaranteed to be of
#' length `n_dim` if provided, or share a common length if otherwise left
#' `NULL`.
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
#' @note See [vctrs::vector_recycling_rules] for additional information on
#' how parameters are recycled to a common length.
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
#' prior <- create_prior(unif, lower = -10, upper = 10, .n_dim = 3)
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
#'   names = c("mu", "sigma", "x"),
#'   lower = c(-Inf, 0, -Inf)
#' )
#' @export
create_prior <- function(
  fn,
  names = NULL,
  lower = -Inf,
  upper = Inf,
  .n_dim = NULL,
  .name_repair = c("unique", "universal", "check_unique")
) {
  fn <- as_function(fn)

  prior <- new_ernest_prior(
    fn = fn,
    n_dim = .n_dim,
    names = names,
    lower = lower,
    upper = upper,
    name_repair = .name_repair
  )
  check_prior(prior)
  prior
}

#' Construct an object of class 'ernest_prior'.
#'
#' @param fn The prior transformation function.
#' @param n_dim Number of dimensions.
#' @param names Character vector of variable names.
#' @param lower,upper Numeric vectors of bounds.
#' @param class Character vector of additional S3 classes.
#' @param name_repair Character string indicating how to repair variable names.
#' Options are `"unique"` (default), `"universal"`, or `"check_unique"`.
#' @param ... Dots forwarded from subclasses of ernest_prior.
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
  names = NULL,
  name_repair = c("unique", "universal", "check_unique"),
  ...,
  class = NULL,
  call = caller_env()
) {
  check_function(fn, call = call)
  check_number_whole(n_dim, min = 1, allow_null = TRUE, call = call)
  name_repair <- arg_match0(
    name_repair,
    c("unique", "universal", "check_unique")
  )
  names <- vctrs::vec_cast(names, to = character()) %||% ""
  lower <- vctrs::vec_cast(lower, to = double()) %||% -Inf
  upper <- vctrs::vec_cast(upper, to = double()) %||% Inf
  meta <- vctrs::df_list(
    "names" = names,
    "lower" = lower,
    "upper" = upper,
    .size = n_dim
  )
  meta$names <- vctrs::vec_as_names(
    meta$names,
    repair = name_repair,
    call = call
  )
  n_dim <- vctrs::vec_size(meta$names)
  if (any(meta$lower >= meta$upper)) {
    cli::cli_abort(
      "`lower` must be strictly smaller than `upper`.",
      call = call
    )
  }

  elems <- list2(fn = fn, !!!meta)
  new_elems <- list2(...)
  check_unique_names(c(elems, new_elems))
  structure(
    c(elems, new_elems),
    n_dim = n_dim,
    class = c(class, "ernest_prior")
  )
}

#' Check the validity of a prior transformation function.
#'
#' This function tests that the user-supplied prior transformation function
#' returns finite double vectors/matrices of the correct dimensions and within
#' specified bounds, for both vector and matrix inputs.
#'
#' @param prior The ernest_prior object.
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
#' @returns `NULL` if all checks pass, otherwise an error is raised.
#' @importFrom cli cli_warn
#' @noRd
check_prior <- function(prior, n_tests = 1000, call = caller_env()) {
  n_dim <- attr(prior, "n_dim")
  vec_test <- stats::runif(n_dim)
  vec_result <- prior$fn(vec_test)
  if (!is_double(vec_result)) {
    obj_type <- obj_type_friendly(vec_result)
    cli::cli_abort(
      "`fn` must return a numeric vector, not {obj_type}.",
      call = call
    )
  }
  if (vctrs::vec_size(vec_result) != n_dim) {
    act <- vctrs::vec_size(vec_result)
    cli::cli_abort(
      "`fn` must return a vector of length {n_dim}, not one of length {act}.",
      call = call
    )
  }

  test <- matrix(stats::runif(n_tests * n_dim), nrow = n_tests)
  result <- apply(test, 1, prior$fn) |> t()
  tryCatch(
    for (i in seq_len(n_dim)) {
      if (any(!is_double(result[i, ], finite = TRUE))) {
        cli::cli_abort(
          "`fn` must return vectors that only contain finite values.",
          call = call
        )
      }
      oo_lower <- any(result[i, ] < prior$lower)
      oo_upper <- any(result[i, ] > prior$upper)

      if (oo_lower || oo_upper) {
        bound_str <- if (oo_lower && oo_upper) {
          "`lower` and `upper`"
        } else if (oo_lower) {
          "`lower`"
        } else {
          "`upper`"
        }
        cli::cli_abort("`fn` must respect the {bound_str} bounds.", call = call)
      }
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "`fn` failed a sanity check.",
          "x" = "Input: {pretty(test[i,])}",
          "x" = "Output: {pretty(result[i,])}"
        ),
        parent = cnd,
        call = call
      )
    }
  )
  invisible(NULL)
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
    name <- switch(
      class(x)[[1]],
      "uniform_prior" = "uniform",
      "normal_prior" = "normal",
      "custom"
    )
    cli::cli_text("{name} prior distribution {.cls {class(x)}}")
    cli::cat_line()
    cli::cat_print(tibble::tibble(
      "names" = x$names,
      "lower" = x$lower,
      "upper" = x$upper,
      "mean" = x$mean,
      "sd" = x$sd
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
