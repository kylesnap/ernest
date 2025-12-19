#' Specify a prior distribution for nested sampling
#'
#' Use an R function to specify the prior distribution of parameters for a
#' nested sampling run.
#'
#' @param fn A function. Takes a vector of unit cube coordinates and
#' returns a vector of parameters of the same length.
#' @param names Unique names for each variable in the prior distribution.
#' Optional for non-custom prior distributions.
#' @param lower,upper Numeric vectors. Expected bounds for the
#' parameter vectors after hypercube transformation.
#' @param repair Describes how to repair the vector of `names`. One of
#' `"check_unique"`, `"unique"`, `"universal"`, `"unique_quiet"`,
#' or `"universal_quiet"`.
#' See [vctrs::vec_as_names()] for descriptions of each repair strategy.
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
  fn <- as_function(fn)
  new_ernest_prior(
    fn,
    names,
    lower,
    upper,
    .class = "custom_prior",
    .repair = repair
  )
}

#' Construct an ernest_prior object
#' @noRd
new_ernest_prior <- function(
  fn,
  names,
  lower = NULL,
  upper = NULL,
  .class = NULL,
  .repair = "check_unique"
) {
  names <- vctrs::vec_as_names(names, repair = .repair)
  n_dim <- length(names)
  if (n_dim < 1) {
    cli::cli_abort("`names` must be at least length one, not length {n_dim}.")
  }

  # Wrap the transformation, catching errors in function output.
  force(fn)
  catching_fn <- function(x) {
    out <- try_fetch(
      {
        fn(x)
      },
      error = function(cnd) {
        cli::cli_abort("Error within `ernest_prior`.", parent = cnd)
      }
    )
    out <- vctrs::vec_cast(out, double(), call = NULL)
    if (any(is.nan(out) | is.na(out))) {
      cli::cli_abort(c(
        "`fn` cannot return non-numeric, missing, or `NaN` values.",
        "x" = "Input: {pretty(out)}",
        "y" = "Output: {pretty(out)}"
      ))
    }
    return(out)
  }

  parallel_fn <- function(x) {
    if (is.matrix(x)) {
      t(apply(x, 1, catching_fn))
    } else if (is.vector(x, mode = "numeric")) {
      catching_fn(x)
    } else {
      stop_input_type(x, "a numeric vector or matrix")
    }
  }
  test_matrix <- check_prior(parallel_fn, n_dim)

  bound_msg <- NULL
  if (is.null(lower)) {
    lower <- parallel_fn(rep(0.0, n_dim))
    bound_msg <- "lower"
  }
  if (is.null(upper)) {
    upper <- parallel_fn(rep(1.0, n_dim))
    bound_msg <- if (is.null(bound_msg)) "upper" else "lower/upper"
  }
  bounds <- vctrs::vec_cast_common(
    "lower" = lower,
    "upper" = upper,
    .to = double()
  )
  bounds <- vctrs::vec_recycle_common(!!!bounds, .size = n_dim)

  if (!vctrs::list_all_size(bounds, n_dim)) {
    cli::cli_abort(
      "`lower` and `upper` must be the same length as `names` ({n_dim}).",
      "i" = if (!is.null(bound_msg)) {
        "{bound_msg} boundaries were calculated by {.fn create_prior}."
      }
    )
  }

  if (any(bounds$lower >= bounds$upper)) {
    indx <- which.max(bounds$lower >= bounds$upper)
    cli::cli_abort(c(
      "`lower` must be strictly smaller than `upper`.",
      "x" = paste0(
        "Problem at index {indx}: ",
        "`{bounds$lower[indx]} \U226E {bounds$upper[indx]}`"
      )
    ))
  }
  check_bounds(test_matrix, bounds)
  rm(test_matrix)

  structure(
    list(
      "fn" = parallel_fn,
      "names" = names,
      "lower" = bounds$lower,
      "upper" = bounds$upper
    ),
    n_dim = n_dim,
    body = expr(!!fn),
    class = c(.class, "ernest_prior")
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
    .repair = "unique",
    .class = "composite_prior"
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
  call = caller_env()
) {
  try_fetch(
    {
      x <- matrix(stats::runif(1000 * n_dim), nrow = 1000)
      check_first <- fn(x[1, , drop = TRUE])
      if (length(check_first) != n_dim) {
        cli::cli_abort(paste0(
          "`fn` must return a numeric vector of length {n_dim}, not one of ",
          "length {length(check_first)}."
        ))
      }
      check_all <- fn(x)
      if (!isTRUE(all.equal(check_all[1, , drop = TRUE], check_first))) {
        cli::cli_abort(
          "`fn` can't return different results for matrices and vectors.",
        )
      }
      check_matrix(
        check_all,
        nrow = 1000,
        ncol = n_dim,
        arg = "test matrix"
      )
      return(check_all)
    },
    error = function(cnd) {
      cli::cli_abort(
        "Error while validating the prior.",
        parent = cnd,
        call = call
      )
    }
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
    name <- sub("_prior", "", class(x)[[1]])
    cli::cli_text("{name} prior distribution {.cls {class(x)}}")
    cli::cat_line()
    cli::cli_text("Number of Dimensions: {attr(x, 'n_dim')}")
    cli::cli_text("Names: {x$names}")
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

#' Check test matrix for errors.
check_bounds <- function(test_matrix, bounds, call = caller_env()) {
  oob_low <- any(matrixStats::colMins(test_matrix) < bounds$lower)
  oob_up <- any(matrixStats::colMaxs(test_matrix) > bounds$upper)
  if (any(oob_low | oob_up)) {
    indx_low <- if (any(oob_low)) which.max(oob_low) else NULL
    indx_up <- if (any(oob_up)) which.max(oob_up) else NULL
    cli::cli_abort(
      c(
        paste0(
          "`fn` must return values within the bounds `lower` and `upper`."
        ),
        "x" = if (oob_low) {
          "Expected lower bounds: {pretty(bounds$lower)}."
        },
        "x" = if (oob_low) {
          "Actual lower bounds: {pretty(matrixStats::colMins(test_matrix))}."
        },
        "x" = if (oob_up) {
          "Expected upper bounds: {pretty(bounds$lower)}."
        },
        "x" = if (oob_up) {
          "Actual upper bounds: {pretty(matrixStats::colMins(test_matrix))}."
        }
      ),
      call = call
    )
  }
  invisible(NULL)
}
