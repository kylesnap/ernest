#' Prepare a prior transformation for nested sampling
#'
#' Creates a prior transformation object of class `ernest_prior`, which defines
#' how to map points from the unit hypercube to the parameter space for use in
#' a nested sampling run.
#'
#' @param point_fn,vectorized_fn `[function]`\cr The prior transformation
#' function. Provide either `point_fn` or `vectorized_fn`:
#' * `point_fn`: Should accept a single parameter vector (numeric vector of
#' length equal to the number of parameters) and return a vector in the original
#' parameter space.
#' * `vectorized_fn`: Should accept a matrix of points in the unit hypercube
#' (rows as parameter vectors) and return a matrix of the same shape in the
#' original parameter space.
#' @param names `[character()]`\cr Unique names for each variable in the prior
#' distribution.
#' @param lower,upper `[double()]`\cr Expected lower and upper bounds for the
#' parameter vectors after transformation.
#' @param repair `[character(1)]`\cr Name repair strategy for `names`. One of
#' `"check_unique"`, `"unique"`, `"universal"`, `"unique_quiet"`, or
#' `"universal_quiet"`. See [vctrs::vec_as_names()] for details.
#'
#' @returns `[ernest_prior]`, an object describing the prior transformation,
#' with `names`, `lower`, and `upper` recycled to the same length.
#'
#' @details
#' The prior transformation encodes points in the parameter space as independent
#' and identically distributed points within a unit hypercube. Nested sampling
#' implementations, including ernest, use this transformation to simplify
#' likelihood-restricted prior sampling and avoid unnecessary rejection steps.
#'
#' Provide your prior as a transformation function. For factorisable priors,
#' this can simply transform each value in (0, 1) using the inverse CDF for each
#' parameter. For more complex cases, you can specify hierarchical or
#' conditionally dependent priors.
#'
#' `create_prior` performs regularity checks on your prior function to catch
#' basic errors that may affect nested sampling. The function must take in a
#' vector (or matrix) of points (each between 0 and 1) and return a vector or
#' matrix of the same shape containing only finite values.
#'
#' If your prior depends on additional data, provide these using an anonymous
#' function (see Examples).
#'
#' @note See [vctrs::vector_recycling_rules] for information on how parameters
#' are recycled to a common length.
#'
#' @srrstats {G2.0a, G2.1a} Documents expectations on the vector parameters
#' `lower` and `upper`.
#' @srrstats {BS1.2, BS1.2c} Specifies how to design a prior in ernest, and
#' provides examples.
#'
#' @aliases ernest_prior
#' @family priors
#' @example ./data-raw/EXAMPLE_PRIOR_CLASS.R
#' @export
create_prior <- function(
  point_fn,
  vectorized_fn,
  names,
  lower = -Inf,
  upper = Inf,
  repair = c(
    "unique",
    "universal",
    "check_unique",
    "unique_quiet",
    "universal_quiet"
  )
) {
  interface <- check_exclusive(point_fn, vectorized_fn)
  fn <- switch(
    interface,
    "point_fn" = point_fn,
    "vectorized_fn" = vectorized_fn
  )
  new_ernest_prior(
    fn,
    names,
    lower,
    upper,
    interface = interface,
    .class = "custom_prior",
    .repair = repair
  )
}

#' Internal constructor for ernest_prior objects
#'
#' @param fn The prior transformation from unit hypercube to parameter space.
#' @param names Character vector. Names of parameters.
#' @param lower,upper Lower and upper bounds for each parameter (optional).
#' @param class Additional classes to assign (e.g., "custom_prior").
#' @param repair Name repair strategy for `names`.
#'
#' @return
#' An object of class `ernest_prior`.
#' @noRd
new_ernest_prior <- function(
  fn,
  names,
  lower = -Inf,
  upper = Inf,
  interface = c("point_fn", "vectorized_fn"),
  ...,
  .class = NULL,
  .repair = "check_unique",
  .call = caller_env()
) {
  check_function(fn, call = .call)
  nvar <- length(names)
  if (nvar < 1) {
    cli::cli_abort(
      "`names` must be at least length one, not length {nvar}."
    )
  }
  names <- vctrs::vec_as_names(names, repair = .repair, call = .call)
  bounds <- vctrs::vec_recycle_common(
    !!!vctrs::vec_cast_common(
      "lower" = lower,
      "upper" = upper,
      .to = double(),
      .call = .call
    ),
    ...,
    .size = nvar,
    .call = .call
  )
  if (any(bounds$lower >= bounds$upper)) {
    cli::cli_abort(
      "`lower` bounds must be strictly smaller than `upper` bounds.",
      call = .call
    )
  }
  interface <- arg_match(interface, call = .call)

  force(fn)
  wrapped_fn <- \(x) vectorized_prior(fn, x, interface, nvar)

  check_prior(
    wrapped_fn,
    nvar,
    lower = bounds$lower,
    upper = bounds$upper,
    arg = "prior$fn(x)",
    call = .call
  )

  structure(
    list2(
      "fn" = wrapped_fn,
      "names" = names,
      !!!bounds
    ),
    nvar = nvar,
    body = fn,
    interface = interface,
    class = c(.class, "ernest_prior")
  )
}

#' Apply the prior transformation to a matrix of points in the unit hypercube.
#'
#' @param .fn The prior transformation function.
#' @param x A numeric vector or matrix of points in the unit hypercube.
#' @param interface The interface type of the prior function.
#' @param nvar The number of parameters (dimensions).
#' @return A matrix of transformed points in the original parameter space.
#' @noRd
vectorized_prior <- function(.fn, x, interface, nvar) {
  if (!is_double(x)) {
    stop_input_type(x, "a numeric vector or matrix", call = NULL)
  }
  if (!is.matrix(x)) {
    dim(x) <- c(1, length(x))
  }
  y <- switch(
    interface,
    "point_fn" = do.call(rbind, apply(x, 1, .fn, simplify = FALSE)),
    "vectorized_fn" = .fn(x)
  )
  vec_cast(y, to = matrix(double(), nrow = nrow(x), ncol = nvar))
}


#' Check the validity of a prior transformation function.
#'
#' This function tests that the user-supplied prior transformation function
#' returns finite double vectors/matrices of the correct dimensions and within
#' specified bounds, for both vector and matrix inputs.
#'
#' @param fn The prior transformation function.
#' @param nvar Dimensionality.
#' @param lower,upper Numeric vectors. Expected bounds for the
#' parameter vectors after hypercube transformation.
#' @param arg Argument name for error reporting.
#' @param call Environment for error reporting.
#'
#' @srrstats {G2.0, G2.1} Uses vctrs functions to ensure that the inputs are of
#' the commensurate size and type.
#' @srrstats {G2.4, G2.4a, G2.4b} Explicit conversion of inputs to expected
#' types or error messages for univariate parameters.
#' @srrstats {G2.4c} Ensures that names is a unique character string using
#' vctrs.
#' @srrstats {BS2.2, BS2.3} Ensures that the lengths of the prior parameters are
#' validated before the NS algorithm is invoked.
#'
#' @returns The test matrix if all checks pass.
#' @noRd
check_prior <- function(
  fn,
  nvar,
  lower = -Inf,
  upper = Inf,
  arg = caller_arg(fn),
  call = caller_env()
) {
  # Vector input sanity check (single point)
  test_vector <- rep(0.5, nvar)
  output_test <- fn(test_vector)

  # 2) Matrix input sanity check (batched points)
  test_matrix <- matrix(stats::runif(1000 * nvar), nrow = 1000)
  output_test_mat <- fn(test_matrix)
  if (any(!is.finite(output_test_mat))) {
    cli::cli_abort("`{arg}` must return only finite values.", call = call)
  }

  if (identical(lower, -Inf) && identical(upper, Inf)) {
    return(output_test_mat)
  }

  # Bounds checks (inclusive, matching prior check_matrix behavior)
  bounds <- vctrs::vec_recycle_common(
    lower = lower,
    upper = upper,
    .size = nvar,
    .call = call
  )
  mins <- matrixStats::colMins(output_test_mat)
  maxes <- matrixStats::colMaxs(output_test_mat)
  if (any(mins < bounds$lower)) {
    cli::cli_abort("`{arg}` must respect the lower bounds.", call = call)
  }
  if (any(maxes > bounds$upper)) {
    cli::cli_abort("`{arg}` must respect the upper bounds.", call = call)
  }
  output_test_mat
}

#' Combine two ernest_prior objects
#' @rdname create_prior
#' @param x,y [[ernest_prior]]\cr Prior objects to combine.
#'
#' @export
`+.ernest_prior` <- function(x, y) {
  check_class(x, "ernest_prior")
  check_class(y, "ernest_prior")

  nvar <- c(attr(x, "nvar"), attr(y, "nvar"))
  bodies <- list(attr(x, "body"), attr(y, "body"))
  cum_dim <- cumsum(nvar)

  fn <- new_function(
    exprs(x = ),
    expr({
      vec_c(
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

#' Pretty formattted string for prior
#' @param x An object of class 'ernest_prior'.
#' @param ... Ignored.
#' @returns A string.
#' @noRd
format.ernest_prior <- function(x, ...) {
  cli::format_inline("{.cls {class(x)}} ({attr(x, 'nvar')} dims.)")
}

#' Print for ernest_prior
#'
#' @param x An object of class 'ernest_prior'.
#' @param ... Ignored.
#'
#' @returns `x`, invisibly.
#' @noRd
#' @export
print.ernest_prior <- function(x, ...) {
  cli::cli_text("{format.ernest_prior(x,...)}")

  cli::cli_code(expr_deparse(attr(x, "body")), ...)
  cli::cli_bullets(c(
    "v" = "Names: {.val {x$names}}",
    "v" = "Interface: {.val {attr(x, 'interface')}}"
  ))
  invisible(x)
}
