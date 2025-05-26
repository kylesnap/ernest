#' Wrap a Log-Likelihood Function for Ernest
#'
#' @description
#' This generic function wraps a log-likelihood function or a fitted model
#' object (e.g., from [stats::glm()]) for use with the `ernest` package. It
#' ensures compatibility with the nested sampling framework by standardizing the
#' log-likelihood computation.
#'
#' @param object An object representing the log-likelihood function, specified in
#' one of the following ways:
#' * A named function, e.g. `mean`.
#' * An anonymous function, e.g. `\(x) x + 1` or `function(x) x + 1`.
#' * A fitted model object from [glm()].
#' @inheritParams rlang::args_dots_empty
#'
#' @return An object of class `ernest_likelihood`, which is a function that
#' computes the log-likelihood of the data given a set of parameters.
#'
#' @examples
#' # Example with a custom log-likelihood function
#' log_lik_fn <- function(x) -sum((x - 1)^2)
#' wrapped_fn <- ernest_likelihood(log_lik_fn)
#' @export
ernest_likelihood <- function(object, ...) {
  UseMethod("ernest_likelihood")
}

#' @rdname ernest_likelihood
#' @export
ernest_likelihood.default <- function(object, ...) {
  stop_input_type(
    object,
    "a function or a fitted model object (e.g., from `glm()`)"
  )
}

#' @rdname ernest_likelihood
#' @export
ernest_likelihood.function <- function(object, ...) {
  check_dots_empty()
  fn <- as_function(object)
  new_ernest_likelihood(fn, body = object)
}

#' @rdname ernest_likelihood
#' @importFrom stats family model.matrix model.response
#' @importFrom stats model.offset update model.weights
#' @export
ernest_likelihood.glm <- function(object, ...) {
  if (is.null(object$model)) {
    object <- update(object, model = TRUE)
  }

  y <- unname(model.response(object$model))
  x <- model.matrix(object)
  weights <- model.weights(object$model)
  family <- family(object)

  fn <- switch(
    family$family,
    "gaussian" = gaussian_lpdf(y, x),
    "binomial" = {
      parsed_y <- parse_binomial_response(y, weights)
      binomial_lpmf(parsed_y, x, family$linkinv)
    },
    cli::cli_abort("The {family$family} family is not supported.")
  )

  new_ernest_likelihood(fn, body = object$call)
}

#' Internal method: Gaussian log-likelihood
#' @section Internal
#' @noRd
gaussian_lpdf <- function(y, x) {
  y <- as.double(y)
  x <- as.matrix(x)
  n <- length(y)
  n_free <- ncol(x) + 1
  coef_idx <- seq_len(ncol(x))

  function(theta) {
    mu <- drop(x %*% theta[coef_idx])
    sigma <- theta[n_free]

    y_scaled <- (y - mu) / sigma
    y_scaled_sq_sum <- sum(y_scaled^2)
    -0.5 * n * log(2 * pi) - sum(log(sigma) * n) - 0.5 * y_scaled_sq_sum
  }
}

#' Internal method: Binomial log-likelihood
#' @section Internal
#' @noRd
binomial_lpmf <- function(parsed_y, x, linkinv) {
  # Helper functions
  log_linkinv <- \(eta) log(linkinv(eta))
  log1m_linkinv <- \(eta) log1p(-linkinv(eta))

  # Broadcast scalars to vectors if necessary
  y <- as.integer(parsed_y$y)
  size <- as.integer(parsed_y$size)

  function(theta) {
    # Calculate linear predictor
    eta <- drop(x %*% theta)

    # Logit binomial log-likelihood calculation
    log_inv <- log_linkinv(eta)
    log1m_inv <- log1m_linkinv(eta)
    logp <- sum(y * log_inv + (size - y) * log1m_inv)
    logp + sum(lchoose(size, y))
  }
}

#' Internal method to parse binomial response
#' @section Internal
#' @noRd
parse_binomial_response <- function(y, weights) {
  if (is.matrix(y)) {
    weights <- rowSums(y)
    y <- y[,1]
  } else if (!is_empty(weights)) {
    y <- round(weights * y)
  }
  weights <- as.integer(weights %||% rep(1, length(y)))
  list(y = y, size = weights)
}

#' Create a new `ernest_likelihood` object
#' @noRd
new_ernest_likelihood <- function(fn, body) {
  structure(
    fn,
    body = expr_deparse(body),
    class = c("ernest_likelihood", "function")
  )
}

#' @noRd
#' @export
format.ernest_likelihood <- function(x, ...) {
  check_dots_empty()
  cli::cli_format_method({
    cli::cli_h3("Ernest-Wrapped Log-Likelihood Function")
    cli::cat_print(attr(x, "call"))
  })
}

#' @noRd
#' @export
print.ernest_likelihood <- function(x, ...) {
  cat(format(x), sep = "\n")
}
