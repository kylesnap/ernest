#' Wrap a Log-Likelihood Function for Ernest
#'
#' @param object A function or a fitted model object from `glm()`.
#' @param ... Additional arguments passed to the method. These are currently ignored.
#'
#' @return An object of class `ernest_likelihood`, which is a function that
#' computes the log-likelihood of the data given a set of parameters. If the object
#' is of class `glm`, it will also include the original call, [family()], and [terms()] object.
#'
#' @export
ernest_likelihood <- function(object, ...) {
  UseMethod("ernest_likelihood")
}

#' @rdname ernest_likelihood
#' @export
ernest_likelihood.function <- function(object, ...) {
  fn <- as_function(object)
  if (length(rlang::fn_fmls(fn)) != 1) {
    cli::cli_abort("`object` must be a function of exactly one argument.")
  }
  new_ernest_likelihood(fn, call = fn_body(fn))
}

#' @rdname ernest_likelihood
#' @export
ernest_likelihood.glm <- function(object, ...) {
  if (is.null(object$model)) {
    object <- update(object, model = TRUE)
  }

  y <- unname(model.response(object$model))
  x <- model.matrix(object)
  nobs <- nobs(object)
  p <- ncol(x)
  off <- model.offset(object$model)
  prior_weights <- model.weights(object$model)

  # In the case of binomial data, parse the response object
  non_constant_weight <- all(prior_weights == 1)
  intercept <- attr(object$terms, "intercept")

  family <- family(object)
  linkinv <- family$linkinv

  eta <- expr(!!x %*% theta[!!c(1:p)])
  mu <- call2(linkinv, eta)
  dispersion <- expr(theta[!!(p + 1)])

  dfun <- switch(
    family$family,
    "gaussian" = {
      if (!is_empty(prior_weights)) {
        expr(dnorm(!!y, mean = !!mu, sd = sqrt(!!dispersion / !!prior_weights), log = TRUE))
      } else {
        expr(dnorm(!!y, mean = !!mu, sd = sqrt(!!dispersion), log = TRUE))
      }
    },
    "binomial" = {
      response <- parse_binomial_response(y, prior_weights)
      expr(dbinom(!!response$y, size = !!response$size, prob = !!mu, log = TRUE))
    },
    # TODO: Add support for other families
    # "Gamma" = {
    #   if (!is_empty(prior_weights)) {
    #     cli::cli_inform("Using prior weights in the shape parameters.")
    #     expr(dgamma(!!y, shape = !!prior_weights/!!dispersion, scale = !!mu * !!dispersion, log = TRUE))
    #   } else {
    #     expr(dgamma(!!y, shape = 1 / !!dispersion, scale = !!mu * !!dispersion, log = TRUE))
    #   }
    # },
    # "poisson" = {
    #   if (!is_empty(prior_weights)) {
    #     cli::cli_warn("`prior weights` will be ignored for the poisson family.")
    #   }
    #   expr(dpois(!!y, lambda = !!fitted_values, log = TRUE))
    # },
    cli::cli_abort("The {family$family} family is not supported.")
  )

  fn <- new_function(
    exprs(theta = ),
    expr(sum(!!dfun))
  )
  new_ernest_likelihood(
    fn,
    call = object$call,
    family = object$family,
    terms = object$terms
  )
}

#' @noRd
parse_binomial_response <- function(y, weights) {
  weights <- if (is_empty(weights)) {
    rep(1L, length(y))
  } else {
    vctrs::vec_cast(weights, integer())
  }
  if (is.matrix(y) && ncol(y)) {
    prior_weights <- as.integer(rowSums(y))
    y <- y[, 1]
    return(list(y = y, size = prior_weights))
  }
  if (is.factor(y)) {
    y <- as.integer(y) - 1
    return(list(y = drop(y), size = 1L))
  }
  return(list(y = y * weights, size = weights))
}

#' @noRd
new_ernest_likelihood <- function(fn, call, family = NULL, terms = NULL) {
  structure(
    fn,
    class = c("ernest_likelihood", "function"),
    call = call,
    family = family,
    terms = terms
  )
}

#' @export
format.ernest_likelihood <- function(x, ...) {
  check_dots_empty()
  cli::cli_format_method({
    cli::cli_h3("Ernest-Wrapped Log-Likelihood Function")
    cli::cat_print(attr(x, "call"))
  })
}

#' @export
print.ernest_likelihood <- function(x, ...) {
  cat(format(x), sep = "\n")
}
