#' Specify a (Log) Likelihood Function
#'
#' Create a standardized log-likelihood function for use by ernest.
#'
#' @param x A log-likelihood function, or an object inheriting from `glm`,
#'   such as one from a call to [glm()].
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' A function of class `create_likelihood`, which calculates the
#' log-likelihood of a model at a given parameter vector. The function
#' is guaranteed to return a finite value, `-Inf`, or an error.
#' @export
create_likelihood <- function(object) {
  UseMethod("create_likelihood")
}

#' @noRd
#' @export
create_likelihood.ernest_likelihood <- function(object, ...) {
  check_dots_empty()
  create_likelihood.function(attr(object, "body"))
}

#' @rdname create_likelihood
#' @export
create_likelihood.function <- function(object, ...) {
  check_dots_empty()
  x <- as_function(object)

  new_ernest_likelihood(object)
}

#' @rdname create_likelihood
#' @export
create_likelihood.glm <- function(object, ...) {
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

  new_ernest_likelihood(fn)
}

#' Internal method: Gaussian log-likelihood
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
#' @noRd
parse_binomial_response <- function(y, weights) {
  if (is.matrix(y)) {
    weights <- rowSums(y)
    y <- y[, 1]
  } else if (!is_empty(weights)) {
    y <- round(weights * y)
  }
  weights <- as.integer(weights %||% rep(1, length(y)))
  list(y = y, size = weights)
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
new_ernest_likelihood <- function(fn) {
  wrapped <- new_function(
    exprs(point = ),
    quote(tryCatch(
      {
        ll <- fn(point)
        if (ll == Inf | is.nan(ll) | is.na(ll) | !is.numeric(ll)) {
          cli::cli_warn(c(
            "`ernest_likelihood` must only return finite values or {-Inf}.",
            "i" = "Transformed ({style_vec(point)}) = {.arg {ll}}  to `-Inf`."
          ))
          -Inf
        } else {
          ll
        }
      },
      error = function(e) {
        cli::cli_abort(
          "Couldn't calculate the log-lik. of ({style_vec(point)}).",
          parent = e
        )
      }
    ))
  )

  structure(
    wrapped,
    body = expr(!!fn),
    class = c("ernest_likelihood", "function")
  )
}

#' @noRd
#' @export
format.ernest_likelihood <- function(x, ...) {
  check_dots_empty()
  cli::cli_format_method({
    cli::cat_print(attr(x, "body"))
  })
}

#' @noRd
#' @export
print.ernest_likelihood <- function(x, ...) {
  cat(format(x), sep = "\n")
}
