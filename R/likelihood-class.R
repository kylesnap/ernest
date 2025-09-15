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
#' @examples
#' # A 3D Gaussian likelihood function
#' n_dim <- 3
#' sigma <- diag(0.95, nrow = 3)
#' det_sigma <- determinant(sigma, logarithm = TRUE)$modulus
#' attributes(det_sigma) <- NULL
#' prec <- solve(sigma)
#' log_norm <- -0.5 * (log(2 * pi) * n_dim + det_sigma)
#'
#' fn <- function(theta) {
#'   drop(-0.5 * crossprod(theta, crossprod(prec, theta)) + log_norm)
#' }
#' log_lik <- create_likelihood(fn)
#' log_lik(c(0, 0, 0))
#'
#' # Bind data to the likelihood function an anonymous function.
#' y <- 100000000 * runif(11, min = 0.1, max = 0.3)
#' log_lik <- function(theta, y) {
#'   if (theta[2] <= 0) {
#'     return(-Inf)
#'   }
#'   sum(dnorm(y, mean = theta[1], sd = theta[2], log = TRUE))
#' }
#' create_likelihood(\(theta) log_lik(theta, y))
#' @aliases ernest_likelihood
#' @export
create_likelihood <- function(fn, on_nonfinite = c("warn", "quiet", "abort")) {
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
  on_nonfinite = c("warn", "quiet", "abort"),
  call = caller_env()
) {
  fn <- as_function(fn, call = call)
  on_nonfinite <- arg_match(on_nonfinite, call = call)

  nonfinite_expr <- switch(
    on_nonfinite,
    "warn" = expr({
      cli::cli_warn("Replacing `{code}` with `-Inf`.")
      code <- -Inf
    }),
    "quiet" = expr(code <- -Inf),
    "abort" = expr(
      cli::cli_abort(c(
        "log-lik. values must be either finite or `-Inf`, not {code}."
      ))
    )
  )
  structure(
    function(unit) {
      safe_log_lik(fn(unit), nonfinite_expr)
    },
    body = fn,
    class = c("ernest_likelihood", class(fn))
  )
}

#' Checks evaluated code for errors and nonfiniteness
#' @param code Result of calling log_lik_fn(unit)
#' @param error_expr Expression of what to do when an error is detected.
#' @return `code`, which is either an -Inf or a scalar double, or the result
#' of evaluating `error_expr`.
#' @noRd
safe_log_lik <- function(code, error_expr) {
  code <- vctrs::vec_cast(code, to = double(), x_arg = "log-lik.")
  if (vctrs::vec_size(code) != 1L) {
    eval(error_expr)
  }
  if (is.na(code) || (!is.finite(code) & code != -Inf)) {
    eval(error_expr)
  }
  code
}

#' @noRd
#' @export
format.ernest_likelihood <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_text("likelihood function {.cls ernest_likelihood}")
    cli::cat_line()
    fn <- attr(x, "body")
    cli::cli_code(format(fn))
  })
}

#' @noRd
#' @export
print.ernest_likelihood <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
