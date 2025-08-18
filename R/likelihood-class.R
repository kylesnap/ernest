#' Prepare a likelihood function for nested sampling
#'
#' Creates a modified version of a log. likelihood function that always returns
#' either a finite value or `-Inf` for each vector of parameters that is
#' provided.
#'
#' @param fn,rowwise_fn Pick one of `fn` or `rowwise_fn`:
#' * `fn`: A function. Takes in vectors of parameters and returns a scalar
#' likelihood value (either a finite double or `-Inf`).
#' * `rowwise_fn`: A function. Takes in a matrix of parameters and returns
#' a vector of likelihood values (which are all finite doubles or `-Inf`).
#' @param ... Named arguments to `fn` or `rowwise_fn` that should be partially
#' applied (see [purrr::partial()]).
#' @param .nonfinite_action (case-sensitive string) Action to perform when `fn`
#' passes a value that is nonfinite and non-`-Inf` (e.g, `NaN`, `NA`, or `Inf`).
#' * `"warn"`: Issue a warning and replace values with `-Inf`.
#' * `"quiet"`: Silently replace values with `-Inf`.
#' * `"abort"`: Stop execution and signal an error.
#'
#' @returns
#' A function with additional class `ernest_likelihood`. This function is
#' wrapped in checks that promote type- and size-stability:
#' * If provided a vector of doubles, the function will return
#' a scalar double, `-Inf`, or an error.
#' * If provided a matrix of doubles, the function will return
#' a vector of doubles and `-Inf` equal to the number of matrix rows,
#' or an error.
#' * If provided anything else, the function will throw an error.
#'
#' @details
#' Model likelihoods should be provided as a log density function. The first
#' argument of `fn` or `rowwise_fn` should be a vector or matrix of parameters,
#' respectively. Other arguments can be forwarded by providing named arguments
#' to `...` or through anonymous functions (see Examples).
#'
#' It is expected that `fn` returns a scalar finite doubles or `-Inf` for each
#' parameter vector.
#'
#' Ernest will wrap `fn` so that it may take in a matrix of parameters. Should
#' you have a more efficient implementation of your likelihood function that
#' can handle vectors and matrices, then consider providing `rowwise_fn`
#' instead.
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
#' @aliases ernest_likelihood
#' @export
create_likelihood <- function(
  fn,
  rowwise_fn,
  ...,
  .nonfinite_action = c("warn", "quiet", "abort")
) {
  which_arg <- check_exclusive(fn, rowwise_fn)
  if (which_arg == "fn") {
    if (inherits(fn, "ernest_likelihood")) {
      unsafe_fn <- attr(fn, "unsafe_fn")
      rowwise <- attr(unsafe_fn, "is_rowwise")
      attr(unsafe_fn, "is_rowwise") <- NULL
      if (rowwise) {
        return(create_likelihood(
          rowwise_fn = unsafe_fn,
          .nonfinite_action = .nonfinite_action
        ))
      } else {
        return(create_likelihood(
          fn = unsafe_fn,
          .nonfinite_action = .nonfinite_action
        ))
      }
    }
    fn <- as_function(fn, arg = "fn")
    if (dots_n(...) != 0L) {
      fn <- purrr::partial(fn, ...)
    }
    attr(fn, "is_rowwise") <- FALSE
    rowwise_fn <- as_rowwise_fn(fn)
  } else {
    rowwise_fn <- as_function(rowwise_fn, arg = "rowwise_fn")
    if (dots_n(...) != 0L) {
      rowwise_fn <- purrr::partial(rowwise_fn, ...)
    }
    fn <- rowwise_fn
    attr(fn, "is_rowwise") <- TRUE
  }
  new_ernest_likelihood(fn, rowwise_fn, .nonfinite_action)
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
  fn,
  rowwise_fn,
  .nonfinite_action,
  .call = caller_env()
) {
  .nonfinite_action <- arg_match(
    .nonfinite_action,
    values = c("warn", "quiet", "abort"),
    error_call = .call
  )
  log_lik_fn <- wrap_loglik(rowwise_fn, .nonfinite_action)

  obj <- structure(
    log_lik_fn,
    unsafe_fn = fn,
    nonfinite_action = .nonfinite_action,
    class = c("ernest_likelihood", "function")
  )
  obj
}

#' @noRd
#' @export
format.ernest_likelihood <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_h3("{.cls ernest_likelihood}")
    fn <- attr(x, "unsafe_fn")
    attr(fn, "is_rowwise") <- NULL
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

wrap_loglik <- function(rowwise_fn, .nonfinite_action) {
  function(x) {
    if (!is.numeric(x)) {
      stop_input_type(x, "a numeric vector or matrix")
    }
    size <- if (is.matrix(x)) NROW(x) else 1L
    y <- rowwise_fn(x)
    y <- vctrs::vec_cast(y, to = double(), x_arg = "log_lik(...)")
    vctrs::vec_check_size(y, size = size, arg = "log_lik(...)")
    nonfinite <- is.na(y) | is.nan(y) | y == Inf
    if (any(nonfinite)) {
      if (.nonfinite_action == "abort") {
        cli::cli_abort(c(
          "`lik(x)` must always return finite double values or `-Inf`.",
          "x" = "`lik(x)` returned {unique(y[nonfinite])}.",
          "i" = "Did you set `.nonfinite_action` with {.fn create_likelihood})?"
        ))
      }
      if (.nonfinite_action == "warn") {
        cli::cli_warn("Replacing `{unique(y[nonfinite])}` with `-Inf`.")
      }
      y[nonfinite] <- -Inf
    }
    y
  }
}
