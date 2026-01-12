#' Prepare a likelihood function for nested sampling
#'
#' Creates a modified version of a log-likelihood function that always returns
#' either a finite value or `-Inf` for each vector of parameters provided.
#'
#' @param fn A function that takes a vector of parameters and returns a
#' scalar log-likelihood value (either a finite double or `-Inf`).
#' @param on_nonfinite A case-sensitive string. Action to perform when `fn`
#' returns a value that is non-finite and not `-Inf` (i.e., `NaN`, `NA`, `Inf`):
#' * `"warn"`: Issue a warning and return `-Inf`.
#' * `"quiet"`: Silently return `-Inf`.
#' * `"abort"`: Stop execution and signal an error.
#'
#' @returns
#' A function with class `ernest_likelihood`. When provided a parameter vector
#' this function will always return either a scalar finite double, the value
#' `-Inf`, or an error message.
#'
#' @details
#' Model likelihoods should be provided as a log-density function. The first
#' argument of `fn` should be a vector of free parameters.
#'
#' If the model likelihood is conditional on some data, then use this step to
#' incorporate this data into your nested sampling run. We recommended using
#' an (anonymous function)[rlang::as_function()] to do this (see Examples).
#'
#' It is expected that the log-likelihood function returns a scalar finite
#' double or `-Inf` for each parameter vector. Non-finite values other than
#' `-Inf`, such as `NaN`, `Inf`, or `NA` (i.e. missing values) are handled
#' with the behavior of `on_nonfinite`.
#'
#' @srrstats {BS1.1, BS3.0} Instructions on how to bind data to likelihood
#' calculation provided here in text and in the example. Also explicitly
#' documents how NA likelihood values are handled.
#' @srrstats {G2.14, G2.14a, G2.14b, G2.14c, G2.15, G2.16} create_likelihood
#' controls the behaviour of the likelihood function when it returns a missing
#' value (see `on_nonfinite`). The same parameter also controls the
#' behaviour when non-finite, non-`-Inf` values are returned.
#'
#' @aliases ernest_likelihood
#' @example ./data-raw/EXAMPLE_LIKELIHOOD.R
#' @export
create_likelihood <- function(fn, on_nonfinite = c("warn", "quiet", "abort")) {
  fn <- as_function(fn)
  new_ernest_likelihood(fn = fn, on_nonfinite = on_nonfinite)
}

#' Construct an internal ernest likelihood function
#'
#' Creates an internal likelihood function with error and nonfinite value
#' handling for use in nested sampling.
#'
#' @param fn A function that computes likelihood values.
#' @param ... Arguments to be forwarded to `fn`.
#' @param on_nonfinite Case-sensitive string, one of `"warn"`, `"quiet"`,
#' or `"abort"`.
#' @param .call Calling environment for error reporting.
#'
#' @srrstats {G2.3, G2.3a, G2.3b} Uses arg_match() to ensure an informative
#' error message is provided when the user provides an invalid value for
#' `on_nonfinite`.
#'
#' @return A function of class `"ernest_likelihood"` and `"function"`.
#' @noRd
new_ernest_likelihood <- function(
  fn,
  parallel = FALSE,
  on_nonfinite = c("warn", "quiet", "abort"),
  call = caller_env()
) {
  check_function(fn, call = call)
  check_bool(parallel, call = call)
  on_nonfinite <- arg_match(on_nonfinite, call = call)
  nonfinite_expr <- switch(
    on_nonfinite,
    "warn" = expr({
      nonfinite_val <- unique(out[nonfinite])
      cli::cli_warn("Replacing `{nonfinite_val}` with `-Inf`.", call = NULL)
      out[nonfinite] <- -Inf
    }),
    "quiet" = expr(out[nonfinite] <- -Inf),
    "abort" = expr({
      nonfinite_val <- unique(out[nonfinite])
      cli::cli_abort(
        "log-lik. values must be either finite or `-Inf`, not {nonfinite_val}.",
        call = NULL
      )
    })
  )

  force(fn)
  x <- NULL
  catching_fn <- new_function(
    exprs(x = ),
    expr({
      out <- fn(x)
      out <- vctrs::vec_cast(out, double(), x_arg = "log_lik(x)", call = NULL)
      if (length(out) < 1L) {
        cli::cli_abort("`log_lik(x)` must not be of length 0.", call = NULL)
      }
      nonfinite <- (out == Inf | is.nan(out) | is.na(out))
      if (any(nonfinite)) {
        !!nonfinite_expr
      }
      out
    })
  )

  parallel_fn <- if (parallel) {
    catching_fn
  } else {
    function(x) {
      if (is.matrix(x)) {
        t(apply(x, 1, catching_fn))
      } else if (is.vector(x, mode = "numeric")) {
        catching_fn(x)
      } else {
        stop_input_type(x, "a numeric vector or matrix")
      }
    }
  }

  structure(
    parallel_fn,
    body = expr(!!fn),
    class = c("ernest_likelihood", class(fn))
  )
}

#' @noRd
#' @export
print.ernest_likelihood <- function(x, ...) {
  cli::cli_text("likelihood function {.cls ernest_likelihood}")
  fn <- attr(x, "body")
  cli::cli_code({
    format(fn)
  })
  invisible(x)
}
