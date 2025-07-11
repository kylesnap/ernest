#' Prepare a likelihood function for nested sampling
#'
#' Creates a modified version of a log. likelihood function that always returns
#' either a finite value or `-Inf` for each vector of parameters that is provided.
#'
#' @param fn A function or an object of class `ernest_likelihood`. If a function,
#' it should take a single numeric vector of parameters and
#' return either a finite log likelihood value or `-Inf`.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' A function with additional class `ernest_likelihood`. On a success, this function
#' is guaranteed to return a finite value or `-Inf`. Non-finite, non-`-Inf` values
#' are coerced to `-Inf` with a warning. Non-numeric returns are reported as errors.
#' @examples
#' # A 3D Gaussian likelihood function
#' n_dim <- 3
#' sigma <- diag(0.95, nrow = 3)
#' det_sigma <- determinant(sigma, logarithm = TRUE)$modulus
#' attributes(det_sigma) <- NULL
#' prec <- solve(sigma)
#' log_norm <- -0.5 * (log(2 * pi) * n_dim + det_sigma)
#'
#' log_lik <- function(theta) {
#'   drop(-0.5 * crossprod(theta, crossprod(prec, theta)) + log_norm)
#' }
#' log_lik(c(0, 0, 0))
#'
#' # ernest catches and warns the user about non-finite, non-`-Inf` values
#' try(log_lik(c(Inf, 0, 0)))
#'
#' # ernest will quit when receiving unexpected values from the likelihood function
#' log_lik_2 <- function(theta) {
#'  if (theta[1] < 1) "Odd value." else log_lik(theta)
#' }
#' log_lik_2(c(1, 0, 0))
#' try(log_lik_2(c(0, 0, 0)))
#' @export
create_likelihood <- function(fn, ...) {
  UseMethod("create_likelihood")
}

#' @noRd
#' @export
create_likelihood.ernest_likelihood <- function(fn, ...) {
  check_dots_empty()
  create_likelihood.function(attr(fn, "body"))
}

#' @noRd
#' @export
create_likelihood.function <- function(fn, ...) {
  check_dots_empty()
  fn <- as_function(fn)

  new_ernest_likelihood(fn)
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
    body = fn,
    class = c("ernest_likelihood", "function")
  )
}

#' @noRd
#' @export
format.ernest_likelihood <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_bullets(c("An {.cls ernest_likelihood} function"))
  })
}

#' @noRd
#' @export
print.ernest_likelihood <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
