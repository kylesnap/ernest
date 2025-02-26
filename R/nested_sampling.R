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
#' @param n_dim Number of parameters returned by `prior_transform` and accepted
#' by `log_lik`.
#' @param sampler An `ernest_lrps` object, which is a list specifying a given
#' likelihood-restricted prior sampler.
#' @param n_points Number of live points to use during the nested sampling run.
#' Defaults to `500L`.
#' @param first_update Number of likelihood calls that are performed using the
#' uniform sampling before swapping to the method specified by `sampler`.
#' Either an integer or a double, in which case the parameter is cast to an
#' integer by `first_update * n_points`. Defaults to `2.0 * n_points`.
#' @param between_update Number of likelihood calls that are performed between
#' updates to the `sampler`. Currently ignored. Either an integer or a double,
#' in which case the parameter is cast to an integer by
#' `first_update * n_points`. Defaults to `1.5 * n_points`.
#' @param verbose Whether to print progress messages to the console. Defaults to
#' the value of `getOption("verbose")`.
#' @param ... Additional arguments passed to the method-specific function.
#'
#' @returns An `ErnestSampler` object that can be passed to `generate` to
#' perform nested sampling.
#'
#' @note Currently, `between_update` will not impact the behaviour of the
#' nested samplers, as both `uniform_cube` and `rwmh_cube` do not support
#' in-run updating.
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
nested_sampling.function <- function(x, prior_transform, n_dim,
                                     sampler = rwmh_cube(),
                                     n_points = 500,
                                     first_update = 2.0,
                                     between_update = 1.5,
                                     verbose = getOption("verbose"),
                                     ...) {
  check_function(x)
  check_function(prior_transform)
  check_number_whole(n_dim, min = 1, allow_infinite = FALSE)
  check_number_whole(n_points, min = 1, allow_infinite = FALSE)
  first_update <- validate_integer_parameter(first_update, n_points, min = 0)
  between_update <- validate_integer_parameter(
    between_update,
    n_points,
    min = 0
  )
  check_logical(verbose)

  if (!inherits(sampler, "ernest_lrps")) {
    cli::cli_abort("sampler must be an ernest_lrps object")
  }
  if (!is.null(sampler$max_loop)) {
    check_number_whole(sampler$max_loop, min = 1, allow_infinite = FALSE)
    options(ernest.max_loop = sampler$max_loop)
  }

  build_sampler(sampler, x, prior_transform, n_dim, n_points,
                first_update, between_update, verbose)
}
