#' Preserve seed for a run
#'
#' @param object An `ernest_sampler` or `ernest_run` object with a `seed`
#' attribute.
#' @param .local_envir Forwarded to withr.
#'
#' @return invisibly `NA`, if the seed is preserved, otherwise the seed bound to
#' `object`.
#' @noRd
preserve_seed <- function(object, .local_envir = parent.frame()) {
  if (is.na(attr(object, "seed"))) {
    withr::local_preserve_seed(.local_envir = .local_envir)
    return(invisible(NA))
  }
  withr::local_seed(attr(object, "seed"), .local_envir = .local_envir)
  invisible(attr(object, "seed"))
}

#' Check the class of an object
#'
#' Validates that an object inherits from at least one of the specified classes.
#'
#' @param x An object to check.
#' @param class A character vector of allowed classes.
#' @param ... Additional arguments passed to error handlers.
#' @param allow_null Logical. If TRUE, allows NULL values.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if `x` inherits from one of the specified
#' classes, otherwise throws an informative error.
#' @noRd
check_class <- function(
  x,
  class,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if ((allow_null && is.null(x)) || inherits_any(x, class)) {
    return(invisible(NULL))
  }

  cls_format <- cli::pluralize(
    "an object with {?class/at least one class from} {class}"
  )
  stop_input_type(
    x,
    cls_format,
    ...,
    allow_na = FALSE,
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

#' Check that an object is a double matrix
#'
#' Validates that an object is a double matrix with specified dimensions
#' and within given bounds.
#'
#' @param x An object to check.
#' @param nrow Expected number of rows.
#' @param ncol Expected number of columns.
#' @param lower Numeric. Exclusive lower bounds, recycled to length `ncol`.
#' @param upper Numeric. Exclusive upper bounds, recycled to length `ncol`.
#' @param finite Logical. If TRUE, checks that all values are finite.
#' @param ... Additional arguments passed to error handlers.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if `x` is a valid double matrix,
#' otherwise throws an informative error.
#' @noRd
check_matrix <- function(
  x,
  nrow,
  ncol,
  lower = -Inf,
  upper = Inf,
  finite = TRUE,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!is.matrix(x)) {
    stop_input_type(x, "a matrix", arg = arg, call = call)
  }

  if (typeof(x) != "double") {
    stop_input_type(
      x,
      "a double matrix",
      arg = arg,
      call = call
    )
  }

  if (nrow(x) != nrow) {
    cli::cli_abort("`{arg}` must have {nrow} rows, not {nrow(x)}.", call = call)
  }
  if (ncol(x) != ncol) {
    cli::cli_abort(
      "`{arg}` must have {ncol} columns, not {ncol(x)}.",
      call = call
    )
  }

  if (finite && any(!is.finite(x))) {
    cli::cli_abort(
      "`{arg}` must contain no nonfinite values.",
      call = call
    )
  }

  if (identical(lower, -Inf) && identical(upper, Inf)) {
    return(invisible(NULL))
  }

  bounds <- vctrs::vec_recycle_common(
    lower = lower,
    upper = upper,
    .size = ncol
  )
  mins <- matrixStats::colMins(x)
  maxes <- matrixStats::colMaxs(x)

  if (any(bounds$lower > mins)) {
    cli::cli_abort("`{arg}` must respect the lower bounds.")
  }
  if (any(bounds$upper < maxes)) {
    cli::cli_abort("`{arg}` must respect the upper bounds.")
  }
  invisible(NULL)
}

#' Check that a list has unique, non-empty names
#'
#' Validates that all elements of a list are named and that names are unique.
#'
#' @param x A list to check.
#' @param ... Additional arguments passed to error handlers.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if all names are unique and non-empty,
#' otherwise throws an informative error.
#' @noRd
check_unique_names <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  nms <- vctrs::vec_names(x)
  if (is.null(nms) != any(nms == "")) {
    cli::cli_abort(
      "All elements of `{arg}` must have unique names.",
      call = call
    )
  }

  if (vctrs::vec_duplicate_any(nms)) {
    idx <- vctrs::vec_duplicate_id(nms) |> unique()
    cli::cli_abort(
      c(
        "All elements of `{arg}` must have unique names.",
        "x" = "Repeated names: {nms[idx]}"
      ),
      call = call
    )
  }

  invisible(NULL)
}

#' Inject a list into a vector
#'
#' Concatenates the elements of a list into a single vector.
#'
#' @param x A list to concatenate.
#'
#' @return A vector containing all elements of the list.
#' @noRd
list_c <- function(x) {
  inject(c(!!!x))
}

#' Vectorize a function
#'
#' @param fn A function that accepts a single parameter vector.
#'
#' @return A vectorized version of `fn` that accepts a matrix of parameter
#' vectors.
#' @noRd
vectorize_function <- function(fn) {
  force(fn)
  function(X) {
    if (is.vector(X)) {
      fn(X)
    } else {
      Y <- apply(X = X, 1, fn)
      t(Y)
    }
  }
}
