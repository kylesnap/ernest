#' Prepare a likelihood function for nested sampling
#'
#' Creates a modified version of a log-likelihood function that always returns
#' either a finite value or `-Inf` for each vector of parameters provided.
#'
#' @param fn A function that takes a vector of parameters and returns a
#' scalar log-likelihood value (either a finite double or `-Inf`).
#' @param rowwise_fn `r lifecycle::badge('deprecated')` No longer supported;
#' instead, use `fn` to provide a log-likelihood function.
#' @param ... Named arguments to be partially applied to `fn`.
#' @param .nonfinite_action A case-sensitive string. Action to perform when `fn`
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
#' If the model likelihood is conditional on some data, then incorporate this
#' data into the likelihood function here. You can either build an anonymous
#' function (see [rlang::as_function()]), or use the `...` parameters to
#' partially apply data to `fn` (see [purrr::partial()]).
#'
#' It is expected that the log-likelihood function returns a scalar finite
#' double or `-Inf` for each parameter vector. Non-finite values other than
#' `-Inf`, such as `NaN`, `Inf`, or `NA` (i.e. missing values) are handled
#' with the behavior of `.nonfinite_action`.
#'
#' @srrstats {BS1.1, BS3.0} Instructions on how to bind data to likelihood
#' calculation provided here in text and in the example. Also explicitly
#' documents how NA likelihood values are handled.
#' @srrstats {G2.14, G2.14a, G2.14b, G2.14c, G2.15, G2.16} create_likelihood
#' controls the behaviour of the likelihood function when it returns a missing
#' value (see `.nonfinite_action`). The same parameter also controls the
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
#' # Bind data to the likelihood function using dots or anonymous functions.
#' y <- 100000000 * runif(11, min = 0.1, max = 0.3)
#' log_lik <- function(theta, y) {
#'   if (theta[2] <= 0) {
#'     return(-Inf)
#'   }
#'   sum(dnorm(y, mean = theta[1], sd = theta[2], log = TRUE))
#' }
#' create_likelihood(log_lik, y = !!y)
#' create_likelihood(\(theta) log_lik(theta, y))
#' @aliases ernest_likelihood
#' @export
create_likelihood <- function(
  fn,
  rowwise_fn = deprecated(),
  ...,
  .nonfinite_action = c("warn", "quiet", "abort")
) {
  if (lifecycle::is_present(rowwise_fn)) {
    lifecycle::deprecate_warn(
      "1.1.0",
      "ernest::create_likelihood(rowwise_fn =)",
      "ernest::create_likelihood(fn =)"
    )
    fn <- rowwise_fn
  }

  if (inherits(fn, "ernest_likelihood")) {
    return(create_likelihood(
      as_closure(attr(fn, "bare")),
      ...,
      .nonfinite_action = .nonfinite_action
    ))
  }

  new_ernest_likelihood(
    fn = fn,
    ...,
    .nonfinite_action = .nonfinite_action
  )
}

#' Construct an internal ernest likelihood function
#'
#' Creates an internal likelihood function with error and nonfinite value
#' handling for use in nested sampling.
#'
#' @param fn A function that computes likelihood values.
#' @param ... Arguments to be forwarded to `fn`.
#' @param .nonfinite_action Case-sensitive string, one of `"warn"`, `"quiet"`,
#' or `"abort"`.
#' @param .call Calling environment for error reporting.
#'
#' @srrstats {G2.3, G2.3a, G2.3b} Uses arg_match() to ensure an informative
#' error message is provided when the user provides an invalid value for
#' `.nonfinite_action`.
#'
#' @return A function of class `"ernest_likelihood"` and `"function"`.
#' @noRd
new_ernest_likelihood <- function(
  fn,
  ...,
  .nonfinite_action = c("warn", "quiet", "abort"),
  .call = caller_env()
) {
  .nonfinite_action <- arg_match(.nonfinite_action, error_call = .call)
  fn <- as_function(fn, call = .call)
  if (dots_n(...) > 0L) {
    fn <- purrr::partial(fn, ...)
  }

  nonfinite_expr <- get_nonfinite_expr(.nonfinite_action)
  x <- NULL
  safely <- new_function(
    exprs(x = ),
    expr({
      y <- fn(x)
      if (!is_scalar_double(y) && !isTRUE(is.na(y))) {
        y_str <- obj_type_friendly(y)
        cli::cli_abort(
          "{.cls ernest_likelihood}: `fn(x)` must return a double, not {y_str}."
        )
      }
      if (y == Inf || is.na(y) || is.nan(y)) {
        !!nonfinite_expr
      }
      y
    })
  )

  structure(
    safely,
    bare = fn,
    nonfinite_action = .nonfinite_action,
    class = c("ernest_likelihood", class(fn))
  )
}

#' @noRd
#' @export
format.ernest_likelihood <- function(x, ...) {
  cli::cli_format_method({
    fn <- attr(x, "bare")
    cli::cli_text("Log-likelihood function {.cls ernest_likelihood}")
    if (inherits(fn, "purrr_function_partial")) {
      cli::cat_print(fn)
    } else {
      cli::cli_code(format(fn))
    }
  })
}

#' @noRd
#' @export
print.ernest_likelihood <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Return the error-handling expression for log-lik.
#'
#' @param .nonfinite_action Character string, one of `"warn"`, `"quiet"`, or
#' `"abort"`.
#' @return An expression.
#' @noRd
get_nonfinite_expr <- function(.nonfinite_action) {
  switch(
    .nonfinite_action,
    "warn" = expr({
      cli::cli_warn("Replacing `{y}` with `-Inf`.")
      y <- -Inf
    }),
    "quiet" = expr(y <- -Inf),
    "abort" = expr(
      cli::cli_abort(c(
        "log-lik. values must be either finite or `-Inf`, not {y}.",
        "i" = "Did you set `.nonfinite_action` with {.fn create_likelihood})?"
      ))
    )
  )
}
