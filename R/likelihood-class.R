#' Prepare a likelihood function for nested sampling
#'
#' Creates a modified version of a log. likelihood function that always returns
#' either a finite value or `-Inf` for each vector of parameters that is provided.
#'
#' @param fn The log-likelihood function, see Details.
#' @param error_action What action to take should `fn` throw an error. `warn`
#' prints a warning message and returns `-Inf`. `abort` throws an error,
#' stopping the nested sampling run.
#' @param nonfinite_action What action to take should `fn` return a value that is
#' not a double or an `-Inf` value (e.g., see [is.finite()]). `warn` prints a
#' warning message and replaces the value with `-Inf`, `pass` silently replaces
#' the value with `-Inf`, and `abort` throws an error, stopping the nested
#' sampling run.
#' @param auto_batch Whether to prepare `fn` so that it may be called with a
#' matrix of parameter values. If `FALSE`, its assumed that `fn` can already
#' produce a vector of likelihood values for a matrix with rows of parameter
#' vectors.
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
#' Model likelihoods should be provided as a log density function. It is expected
#' that `fn` should take in exactly one argument; likelihood functions that
#' take in multiple non-default arguments should be entered as anonymous
#' functions (see [rlang::as_function()]).
#'
#' It is expected that `fn` returns a scalar finite values or `-Inf` for each
#' parameter vector. Use `error_action` and `nonfinite_action` to decide on
#' how `ernest_likelihood` handles errors, non-finite, and non-numeric return
#' values from `fn`.
#'
#' As default, `auto_batch` expects that `fn` is incapable of handling matrices
#' of parameter values. It resolves this by wrapping `fn` in a call to
#' [base::apply()]. Should you have a more efficient implementation of your
#' likelihood function, then consider setting `auto_batch == FALSE`.
#'
#' @srrstats {BS2.14, BS2.15} create_likelihood controls the behaviour for
#' handling errors and warnings in the calculation of a nested sampling run.
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
  error_action = c("abort", "warn"),
  nonfinite_action = c("warn", "pass", "abort"),
  auto_batch = TRUE,
  ...
) {
  check_dots_empty()
  create_likelihood.function(
    attr(fn, "body"),
    error_action,
    nonfinite_action,
    auto_batch
  )
}

#' @rdname create_likelihood
#' @export
create_likelihood.function <- function(
  fn,
  error_action = c("abort", "warn"),
  nonfinite_action = c("warn", "pass", "abort"),
  auto_batch = TRUE,
  ...
) {
  check_dots_empty()
  fn <- as_function(fn, env = global_env())
  new_ernest_likelihood(fn, error_action, nonfinite_action, auto_batch)
}

#' Create a new `ernest_likelihood` object
#'
#' @param fn Incoming function.
#' @param call The call that created the likelihood function. Either the
#' user-inputted function, or the call component of the model.
#'
#' @return A new `ernest_likelihood` object, which is a function that takes in
#' a single argument and returns the log-likelihood value for that argument or
#' an error.
#'
#' @noRd
new_ernest_likelihood <- function(
  fn,
  error_action,
  nonfinite_action,
  auto_batch,
  call = caller_env()
) {
  error_action <- arg_match0(
    error_action,
    values = c("abort", "warn"),
    error_call = call
  )
  nonfinite_action <- arg_match0(
    nonfinite_action,
    values = c("warn", "pass", "abort"),
    error_call = call
  )
  check_bool(auto_batch, call = call)

  error_fn <- switch(
    error_action,
    "warn" = expr({
      if (cnd_inherits(cnd, "nonfinite_nonnumeric")) {
        cli_abort(
          "Can't calculate likelihood without an error.",
          parent = cnd
        )
      }
      cli_warn(
        c(
          "Can't calculate likelihood without an error.",
          "!" = "Replacing values with `-Inf`."
        ),
        parent = cnd
      )
      if (is.matrix(x)) rep(-Inf, nrow(x)) else -Inf
    }),
    "abort" = expr(
      cli_abort(
        "Can't calculate the likelihood without an error.",
        parent = cnd
      )
    )
  )

  nonfinite_fn <- switch(
    nonfinite_action,
    "warn" = expr({
      poor_values <- unique(
        head(y[y == Inf | is.nan(y) | is.na(y) | !is.numeric(y)])
      )
      cli_warn(c(
        "`fn` must return finite numeric values or `-Inf`.",
        "!" = "Replacing {poor_values} with `-Inf`."
      ))
      y[y == Inf | is.nan(y) | is.na(y) | !is.numeric(y)] <- -Inf
    }),
    "pass" = expr(
      y[y == Inf | is.nan(y) | is.na(y) | !is.numeric(y)] <- -Inf
    ),
    "abort" = expr({
      poor_values <- unique(
        head(y[y == Inf | is.nan(y) | is.na(y) | !is.numeric(y)])
      )
      cli_abort(
        "Encountered {poor_values} when evaluating likelihood.",
        class = "nonfinite_nonnumeric"
      )
    })
  )

  x <- NULL
  catching_likelihood <- rlang::new_function(
    exprs(x = ),
    expr(
      rlang::try_fetch(
        {
          y <- fn(x)
          if (any(y == Inf | is.nan(y) | is.na(y) | !is.numeric(y))) {
            !!nonfinite_fn
          }
          y
        },
        error = function(cnd) {
          !!error_fn
        }
      )
    )
  )

  batch_expr <- if (auto_batch) expr(drop(apply(x, 1, fn))) else expr(fn(x))
  batched_likelihood <- rlang::new_function(
    exprs(x = ),
    expr(rlang::try_fetch(
      {
        y <- !!batch_expr
        if (any(y == Inf | is.nan(y) | is.na(y) | !is.numeric(y))) {
          !!nonfinite_fn
        }
        as.double(y)
      },
      error = function(cnd) {
        !!error_fn
      }
    ))
  )

  likelihood <- function(x) {
    if (is.matrix(x)) {
      batched_likelihood(x)
    } else if (is.double(x)) {
      catching_likelihood(x)
    } else {
      stop_input_type(x, "a numeric vector or matrix")
    }
  }

  structure(
    likelihood,
    body = fn,
    error_action = error_action,
    nonfinite_action = nonfinite_action,
    class = c("ernest_likelihood", "function")
  )
}

#' @noRd
#' @export
format.ernest_likelihood <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_bullets(c(
      "An {.cls ernest_likelihood} function",
      ">" = "Error Behaviour: {attr(x, 'error_action')}",
      ">" = "Nonfinite Behaviour: {attr(x, 'nonfinite_action')}"
    ))
    cli::cli_code(format(attr(x, 'body')))
  })
}

#' @noRd
#' @export
print.ernest_likelihood <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
