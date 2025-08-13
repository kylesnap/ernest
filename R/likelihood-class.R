#' Prepare a likelihood function for nested sampling
#'
#' Creates a modified version of a log. likelihood function that always returns
#' either a finite value or `-Inf` for each vector of parameters that is
#' provided.
#'
#' @param fn (uni-variate function) The log-likelihood function (see Details).
#' @param error_action (case-sensitive string) Action to perform once `fn`
#' throws an error.
#' * `"abort"`: Stop execution and signal an error.
#' * `"warn"`: Issue a warning and replace output with `-Inf`.
#' @param nonfinite_action (case-sensitive string) Action to perform when `fn`
#' passes a value that is not a finite double nor `-Inf`.
#' * `"warn"`: Issue a warning and replace values with `-Inf`.
#' * `"pass"`: Silently replace values with `-Inf`.
#' * `"abort"`: Stop execution and signal an error.
#' @param auto_batch (logical) Whether to prepare `fn` so that it may be called
#' with a matrix of parameter values. If `FALSE`, its assumed that `fn`
#' can already produce a vector of likelihood values for a matrix with rows of
#' parameter vectors.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' A function with additional class `ernest_likelihood`. This function
#' will accept vectors of parameters, and matrices of parameters, where
#' each row represents a single parameter vector. `ernest_likelihood` will
#' always produce either scalar or vector doubles containing finite values
#' or `-Inf`; values outside this range are either replaced by `-Inf` or
#' cause errors.
#'
#' @details
#' Model likelihoods should be provided as a log density function. It is
#' expected that `fn` should take in exactly one argument; likelihood functions
#' that take in multiple non-default arguments should be entered as anonymous
#' functions (see [rlang::as_function()]). See vignettes for an example of
#' entering data through an anonymous function.
#'
#' It is expected that `fn` returns a scalar finite values or `-Inf` for each
#' parameter vector. Use `error_action` and `nonfinite_action` to decide on
#' how `ernest_likelihood` handles errors, non-finite, and non-numeric return
#' values from `fn`.
#'
#' As default, `auto_batch` expects that `fn` is incapable of handling matrices
#' of parameter values. It resolves this by wrapping `fn` in a call to
#' [base::apply()]. Should you have a more efficient implementation of your
#' likelihood function, then consider setting `auto_batch` to `FALSE`.
#'
#' @srrstats {BS2.14, BS2.15} create_likelihood controls the behaviour for
#' handling errors and warnings in the calculation of a nested sampling run.
#' @srrstats {G2.3, G2.3a} create_likelihood uses `arg_match` to validate
#' character input.
#' @srrstats {G2.14, G2.14a, G2.14b, G2.14c, G2.15} create_likelihood catches
#' missing values produced during a run and acts upon them based on user-desired
#' behaviour.
#' @srrstats {G2.16} Value handling is also performed for other undefined
#' values (Inf, NaN)
#' @srrstats {BS1.1} Textual information on how to enter data into ernest
#' through anonymous functions.
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
#' # As default, `log_lik` will loudly replace non-finite, non-`-Inf` values.
#' try(log_lik(c(Inf, 0, 0)))
#'
#' # Silence warnings with `nonfinite_action = "pass"`
#' quiet_lik <- create_likelihood(log_lik, nonfinite_action = "pass")
#' quiet_lik(c(Inf, 0, 0))
#' @rdname create_likelihood
#' @export
create_likelihood <- function(fn, ...) {
  UseMethod("create_likelihood")
}

#' @rdname create_likelihood
#' @export
create_likelihood.ernest_likelihood <- function(
  fn,
  ...,
  .nonfinite_action = c("warn", "quiet", "abort")
) {
  check_dots_empty()
  new_ernest_likelihood(
    rowwise_fn = attr(fn, "unwrapped_fn"),
    .nonfinite_action = .nonfinite_action
  )
}

#' @rdname create_likelihood
#' @export
create_likelihood.function <- function(
  fn,
  rowwise_fn,
  ...,
  .nonfinite_action = c("warn", "quiet", "abort")
) {
  which_arg <- check_exclusive(fn, rowwise_fn)
  rowwise_fn <- if (which_arg == "fn") {
    fn <- as_function(fn)
    partialized <- purrr::partial(fn, ...)
    as_rowwise_fn(partialized)
  } else {
    rowwise_fn <- as_function(rowwise_fn)
    partialized <- purrr::partial(rowwise_fn, ...)
    partialized
  }
  new_ernest_likelihood(rowwise_fn, .nonfinite_action)
}

#' Construct an internal ernest likelihood function with error and nonfinite
#' value handling.
#'
#' @param fn A partialized function that computes likelihood values.
#' @param .nonfinite_action Character string, one of `"warn"`, `"pass"`, or
#' `"abort"`.
#' @param .call Calling environment for error reporting.
#'
#' @return A function of class `"ernest_likelihood"` and `"function"`.
#' @noRd
new_ernest_likelihood <- function(
  rowwise_fn,
  .nonfinite_action,
  .call = caller_env()
) {
  .nonfinite_action <- arg_match(
    .nonfinite_action,
    values = c("warn", "quiet", "abort"),
    error_call = .call
  )
  log_lik_fn <- wrap_function(
    rowwise_fn,
    .nonfinite_action = .nonfinite_action,
    .call = .call
  )

  structure(
    log_lik_fn,
    unwrapped_fn = rowwise_fn,
    nonfinite_action = .nonfinite_action,
    class = c("ernest_likelihood", "purrr_function_partial", "function")
  )
}

#' @noRd
#' @export
format.ernest_likelihood <- function(x, ...) {
  cli::cli_format_method({
    nonfinite_str <- attr(x, 'nonfinite_action')
    cli::cli_text(
      "An {.cls ernest_likelihood}: {.arg nonfinite_action} = {nonfinite_str}"
    )
  })
}

#' @noRd
#' @export
print.ernest_likelihood <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' Transform a function so that it can be applied over a matrix of inputs.
#' @param fn Function.
#' @return A function that can be applied over vectors.
#' @noRd
as_rowwise_fn <- function(fn) {
  new_function(
    exprs(... = ),
    expr({
      if (is.vector(..1)) {
        (!!fn)(..1)
      } else if (is.matrix(..1)) {
        apply(..1, 1, (!!fn))
      } else {
        stop_input_type(..1, "a double vector or matrix", arg = "..1")
      }
    })
  )
}

wrap_function <- function(rowwise_fn, .nonfinite_action, .call) {
  nonfinite_expr <- switch(
    .nonfinite_action,
    "warn" = expr({
      cli::cli_warn("Replacing `{unique(y[nonfinite])}` with `-Inf`.")
      y[nonfinite] <- -Inf
    }),
    "abort" = expr(
      cli::cli_abort(c(
        "`lik(theta)` must always return finite double values or `-Inf`.",
        "x" = "`lik(theta)` returned {unique(y[nonfinite])}.",
        "i" = "Should you change `nonfinite_action` from {.val abort}?"
      ))
    ),
    "quiet" = expr(y[nonfinite] <- -Inf)
  )

  new_function(
    exprs(... = ),
    expr({
      if (!is.numeric(..1)) {
        stop_input_type(..1, "a numeric vector or matrix")
      }
      size <- if (is.matrix(..1)) NROW(..1) else 1L
      y <- (!!rowwise_fn)(..1)
      vctrs::vec_check_size(y, size = size, arg = "lik(..1)")
      if (!is.double(y)) {
        cli::cli_abort("`lik(..1)` must always return a double.")
      }
      nonfinite <- (y == Inf | is.nan(y))
      if (any(nonfinite)) {
        !!nonfinite_expr
      }
      y
    })
  )
}
