#' Prepare a likelihood function for nested sampling
#'
#' Creates a modified version of a log-likelihood function that always returns
#' either a finite value or `-Inf` for each vector of parameters provided.
#'
#' @param scalar_fn `[function]`\cr The log-likelihood function. Provide either
#' `scalar_fn` or `vectorized_fn`:
#' * `scalar_fn`: Should accept a parameter as a numeric vector and return a
#' single numeric value representing the log-likelihood, or `-Inf`.
#' * `vectorized_fn`: Should accept a matrix of parameter vectors (rows as
#' samples, columns as elements of the parameter vector) and return a vector of
#' log-likelihoods or `-Inf` values for each row.
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
#' likelihood as a function over a single parameter vector (`scalar_fn`) or over
#' a matrix of parameters (`vectorized_fn`).
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
#' @seealso The [cubature]
#' (https://bnaras.github.io/cubature/articles/cubature.html) package for more
#' examples of scalar and vectorized functions.
#'
#' @aliases ernest_likelihood
#' @example ./data-raw/EXAMPLE_LIKELIHOOD.R
#' @export
create_likelihood <- function(
  scalar_fn,
  vectorized_fn,
  on_nonfinite = c("warn", "quiet", "abort")
) {
  interface <- check_exclusive(scalar_fn, vectorized_fn)
  fn <- switch(
    interface,
    "scalar_fn" = scalar_fn,
    "vectorized_fn" = vectorized_fn
  )
  if (inherits(fn, "ernest_likelihood")) {
    new_ernest_likelihood(
      fn = attr(fn, "body"),
      interface = interface,
      on_nonfinite = on_nonfinite
    )
  } else {
    new_ernest_likelihood(
      fn = fn,
      interface = interface,
      on_nonfinite = on_nonfinite
    )
  }
}

#' Construct an internal ernest likelihood function
#'
#' Creates an internal likelihood function with error and nonfinite value
#' handling for use in nested sampling.
#'
#' @param fn A function that computes likelihood values.
#' @param interface Either "scalar" or "vectorized", indicating whether `fn`
#' accepts a single parameter vector or a matrix of parameter vectors.
#' @param on_nonfinite How to handle non-finite likelihood values.
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
  interface = c("scalar_fn", "vectorized_fn"),
  on_nonfinite = c("warn", "quiet", "abort"),
  call = caller_env()
) {
  check_function(fn, call = call)
  interface <- arg_match(interface, call = call)
  on_nonfinite <- arg_match(on_nonfinite, call = call)

  force(fn)
  vectorized_fn <- switch(
    interface,
    "scalar_fn" = vectorize_function(fn),
    "vectorized_fn" = function(X) {
      if (!is.matrix(X)) {
        dim(X) <- c(1, length(X))
      }
      fn(X)
    }
  )

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

  x <- NULL
  lab <- "log_lik(x)"
  catching_fn <- new_function(
    exprs(x = ),
    expr({
      try_fetch(
        {
          if (!is_double(x)) {
            stop_input_type(x, "a numeric vector or matrix", call = NULL)
          }
          y <- vectorized_fn(x)
          y <- vctrs::vec_cast(drop(y), to = double(), x_arg = lab, call = NULL)
          y_missing <- which(y == Inf | is.nan(y) | is.na(y))
          if (!vctrs::vec_is_empty(y_missing)) {
            !!nonfinite_expr
          }
          y
        },
        error = function(cnd) {
          cli::cli_abort(
            "Couldn't calculate the log-lik of {x}.",
            call = NULL,
            parent = cnd
          )
        }
      )
    })
  )

  structure(
    catching_fn,
    body = expr(!!fn),
    interface = interface,
    on_nonfinite = on_nonfinite,
    class = c("ernest_likelihood", class(fn))
  )
}

#' @noRd
#' @export
print.ernest_likelihood <- function(x, ...) {
  compat_str <- switch(
    attr(x, "interface"),
    "scalar_fn" = "Scalar",
    "vectorized_fn" = "Vectorized"
  )
  cli::cli_text("{compat_str} Log-likelihood Function")
  fn <- attr(x, "body")
  cli::cli_code({
    format(fn)
  })
  invisible(x)
}
