#' Build a Nested Sampler
#'
#' The top-level function constructs an instance of an `ErnestSampler`,
#' containing the necessary information for nested sampling. Currently,
#' `nested_sampling` relies on the user to specify `R` functions for both
#' the log-likelihood and prior transform.
#'
#' @param x Function returning the log-likelihood given a `n_dim`-length
#' vector of parameters.
#' @param prior_transform Function translating a point in the unit cube to the
#' prior parameter space. This function should accept a `n_dim`-length vector
#' of points where each value is in the range \eqn{[0, 1]} and return a
#' same-length vector where each point represents a parameter.
#' @param ptype Either a single integer, a vector of character strings giving
#' variable names, or a zero-row [tibble::tibble()] that defines the name of
#' each variable and the dimensionality of the prior space.
#' @param sampler An `ernest_lrps` object, which is a list specifying a given
#' likelihood-restricted prior sampler.
#' @param n_points Number of live points to use during the nested sampling run.
#' Defaults to `500L`.
#' @param update_interval Number of likelihood calls that are performed between
#' updates to the `sampler`. Either an integer or a double, in which case the
#' parameter is cast to an integer by `first_update * n_points`.
#' @param verbose Whether to print progress messages to the console. Defaults to
#' the value of `getOption("verbose")`.
#' @param ... If `x` is a function, then these must be empty.
#'
#' @returns An `ernest_sampler` object.
#'
#' @export
nested_sampling <- function(x, ...) {
  UseMethod("nested_sampling")
}

#' @rdname nested_sampling
#' @export
nested_sampling.default <- function(x, ...) {
  cli::cli_abort("No method defined for {class(x)}")
}

#' @rdname nested_sampling
#' @export
nested_sampling.function <- function(x,
                                     prior_transform,
                                     ptype,
                                     sampler = rwmh_cube(),
                                     n_points = 500,
                                     update_interval = 1.5,
                                     verbose = getOption("verbose"),
                                     ...) {
  check_function(x)
  check_function(prior_transform)
  ptype <- make_ptype(ptype)
  n_dim <- length(ptype)
  check_number_whole(n_points, min = 1)
  update_interval <- validate_integer_parameter(
    update_interval,
    n_points,
    min = 0
  )
  check_logical(verbose)

  # TODO: This can probably be moved to another function
  if (!inherits(sampler, "ernest_lrps")) {
    cli::cli_abort("sampler must be an ernest_lrps object")
  }
  sampler$log_lik <- x
  sampler$prior_transform <- prior_transform
  sampler$update_interval <- update_interval
  sampler$n_dim <- n_dim
  sampler <- refresh_lrps(sampler)
  ernest_sampler$new(sampler, ptype, n_points, verbose = verbose)
}
