#' Prepare a likelihood function for nested sampling
#'
#' Creates a modified version of a log-likelihood function that always returns
#' either a finite value or `-Inf` for each vector of parameters provided.
#'
#' @param fn,matrix_fn `[function]`\cr A model's log-likelihood function.
#' Provide either `fn` or `matrix_fn`:
#' * `fn`: Should accept a parameter as a numeric vector and return a single
#' numeric value representing the log-likelihood, or `-Inf`.
#' * `matrix_fn`: Should accept a matrix of parameter vectors (rows as samples,
#' columns as elements of the parameter vector) and return a
#' vector of log-likelihoods or `-Inf` values for each row.
#' @param on_nonfinite `[character]`\cr How the sampler should handle values
#' returned by `fn` or `matrix_fn` that are not finite and not equal to `-Inf`.
#' Must be one of:
#' * `"warn"`: Issue a warning and return `-Inf`.
#' * `"quiet"`: Silently return `-Inf`.
#' * `"abort"`: Stop execution and signal an error.
#'
#' @returns `[ernest_likelihood]`, which inherits from `function`.
#'
#' @details
#' Provide model likelihoods as a log-density function, which take a vector of
#' free parameter values and return the corresponding log-likelihood value.
#'
#' Likelihoods are typically the most computationally expensive function to
#' evaluate in a nested sampling run. ernest allows you to implement your
#' likelihood as a function over a single parameter vector (`fn`) or over a
#' matrix of parameters (`matrix_fn`).
#'
#' ernest expects the log-likelihood function to return a
#' finite double or `-Inf` for each parameter vector. The behaviour when
#' encountering non-finite values other than `-Inf` (such as `NaN`, `Inf`, or
#' `NA`) is controlled by `on_nonfinite`.
#'
#' If your log-likelihood depends on additional data (e.g., an observation
#' matrix or data frame), provide these using an
#' (anonymous function)[rlang::as_function()] (see Examples).
#'
#' @srrstats {BS1.1, BS3.0} Instructions for binding data to likelihood
#' calculation are provided here and in the example. Explicitly documents
#' handling of NA likelihoods.
#' @srrstats {G2.14, G2.14a, G2.14b, G2.14c, G2.15, G2.16} `create_likelihood`
#' controls behaviour when the likelihood returns a missing value
#' (see `on_nonfinite`). The same parameter also controls behaviour for
#' non-finite, non-`-Inf` values.
#'
#' @aliases ernest_likelihood
#' @example ./data-raw/EXAMPLE_LIKELIHOOD.R
#' @export
create_likelihood <- function(
  fn,
  matrix_fn,
  on_nonfinite = c("warn", "quiet", "abort")
) {
  arg <- check_exclusive(fn, matrix_fn)
  if (arg == "fn" && inherits(fn, "ernest_likelihood")) {
    return(new_ernest_likelihood(
      attr(fn, "body"),
      matrix_compat = attr(fn, "matrix_compat"),
      on_nonfinite = on_nonfinite
    ))
  }
  matrix_compat <- ifelse(arg == "matrix_fn", "user", "auto")
  fn <- switch(
    arg,
    "fn" = as_function(fn),
    "matrix_fn" = as_function(matrix_fn)
  )
  new_ernest_likelihood(
    fn = fn,
    matrix_compat = matrix_compat,
    on_nonfinite = on_nonfinite
  )
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
  matrix_compat = c("auto", "user"),
  on_nonfinite = c("warn", "quiet", "abort"),
  call = caller_env()
) {
  check_function(fn, call = call)
  matrix_compat <- arg_match(matrix_compat, call = call)
  on_nonfinite <- arg_match(on_nonfinite, call = call)
  nonfinite_expr <- switch(
    on_nonfinite,
    "warn" = expr({
      cli::cli_warn(
        "Replacing `{unique(y[y_missing])}` with `-Inf`.",
        call = NULL
      )
      y[y_missing] <- -Inf
    }),
    "quiet" = expr(y[y_missing] <- -Inf),
    "abort" = expr({
      cli::cli_abort(
        c(
          "log-lik. values must be either finite or `-Inf`.",
          "x" = "Detected non-viable value: `{unique(y[y_missing])}`."
        ),
        call = NULL
      )
    })
  )

  force(fn)
  x <- NULL
  catching_fn <- new_function(
    exprs(x = ),
    expr({
      try_fetch(
        {
          y <- fn(x)
          y <- vctrs::vec_cast(
            y,
            to = double(),
            x_arg = "log_lik(x)",
            call = NULL
          )
          y_missing <- which(is.nan(y) | y == Inf | is.na(y))
          if (!vctrs::vec_is_empty(y_missing)) {
            !!nonfinite_expr
          }
          y
        },
        error = function(cnd) {
          cli::cli_abort(
            "Couldn't calculate the log. lik of {x}.",
            call = NULL,
            parent = cnd
          )
        }
      )
    })
  )

  rowwise_fn <- if (matrix_compat == "auto") {
    function(x) {
      if (!is.matrix(x)) {
        catching_fn(x)
      } else {
        apply(x, 1, catching_fn)
      }
    }
  } else {
    catching_fn
  }

  parallel_fn <- function(x) {
    if (!is_double(x)) {
      stop_input_type(x, "a numeric vector or matrix", call = NULL)
    }
    rowwise_fn(x)
  }

  structure(
    parallel_fn,
    body = expr(!!fn),
    matrix_compat = matrix_compat,
    class = c("ernest_likelihood", class(fn))
  )
}

#' @noRd
#' @export
print.ernest_likelihood <- function(x, ...) {
  compat_str <- if (attr(x, "matrix_compat") == "auto") {
    "Auto-Generated"
  } else {
    "User-Provided"
  }
  cli::cli_text("Log-likelihood Function ({compat_str} Matrix Compatibility)")
  fn <- attr(x, "body")
  cli::cli_code({
    format(fn)
  })
  invisible(x)
}
