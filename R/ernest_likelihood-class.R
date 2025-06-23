#' Specify a (Log) Likelihood Function
#'
#' Create a standardized log-likelihood function for use by ernest.
#'
#' @param object An object of class function or `ernest_likelihood`.
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
