#' Preserve seed for a run
#'
#' @param seed A number, or `NA` to preserve the current seed.
#' @param .local_envir Forwarded to withr.
#'
#' @return `seed`, invisibly.
#' @noRd
preserve_seed <- function(seed, .local_envir = parent.frame()) {
  if (is.na(seed)) {
    withr::local_preserve_seed(.local_envir = .local_envir)
    return(invisible(NA))
  }
  withr::local_seed(seed, .local_envir = .local_envir)
  invisible(seed)
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

# Helpers for computing and reporting results -----
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

#' Log-space subtraction
#'
#' @param a,b Numeric vectors of equal length.
#'
#' @return `log(exp(a) - exp(b))`, computed in log-space to avoid numerical
#' underflow. A warning is issued and `NaN` is returned when `b > a`.
#' @noRd
logspace_sub <- function(a, b) {
  a + log1p(-exp(b - a))
}

#' Fast data frame creation
#'
#' @param ... Columns of the data frame.
#' @param .error_call Call environment for error messages.
#'
#' @source Internally used in ggdist.
#'
#' @return A data frame created with minimal name repair for speed.
#' @noRd
data_frame0 <- function(..., .error_call = current_env()) {
  vctrs::data_frame(..., .name_repair = "minimal", .error_call = .error_call)
}

#' Fast tibble creation
#'
#' @param x A data frame or list of columns.
#' @param ... Additional attributes to add to the tibble.
#' @param class Additional classes to add to the tibble.
#'
#' @source Recommended by ROpenSci standards.
#'
#' @return A tibble-ish object.
#' @noRd
new_tibble0 <- function(x, ..., class = NULL) {
  vctrs::new_data_frame(
    x = x,
    ...,
    class = c(class, "tbl_df", "tbl")
  )
}
