#' Perform Nested Sampling
#'
#' @param x A function returning the log likelihood of a model given some parameters.
#' @param prior_transform The prior distribution for the parameters, provided as
#' a list of `prior_transform` objects.
#' @param sampler The likelihood-restricted prior sampler to use.
#' @param control A list of control parameters for the nested sampling algorithm,
#' whose defaults are specified in `nested_sampling.control()`.
#' @param ... Extra parameters that are passed to the log likelihood function.
#'
#' @returns Lots of stuff about the run.
#' @export
nested_sampling <- function(x, ...) {
  UseMethod("nested_sampling")
}

#' @rdname nested_sampling
#' @export
nested_sampling.function <- function(x, prior_transform, sampler = unit_cube(),
                                     control = list(),...) {
  if (!inherits(prior_transform, "prior_transform")) {
    cli::cli_abort("`prior_transform` must be a `prior_transform` object.")
  }
  sampler <- update_sampler(
    sampler,
    log_lik = x,
    prior_transform = prior_transform,
    num_dim = prior_transform$dim
  )
  control <- do.call(nested_sampling.control, control)
  result <- nested_sampling_impl(sampler, control)
  new_ernest_run(sampler, control, result)
}

#' Auxiliary for Controlling Nested Sampling
#'
#' @param num_points Number of live points. Larger numbers result in a more
#' finely sampled posterior, in exchange for longer run times.
#' @param first_update Number of calls to log-likelihood before the bounding
#' distribution is reengaged. If this is given as a float, it is
#' converted to an integer through `round(update_interval * num_points)`.
#' @param update_interval Number of calls between updates of the bounding
#' behaviour of certain nested samplers. If this is given as a float, it is
#' converted to an integer through `round(update_interval * num_points)`
#' @param max_iter Maximum number of iterations of the nested sampling loop.
#' If left `NA`, the loop will run until another stopping criteria is met.
#' @param max_call Maximum number of likelihood evaluations. If left `NA`, the
#' loop will run until another stopping criteria is met.
#' @param dlogz Nested sampling will run until the remaining prior volume to
#' the total evidence falls below this threshold. If set to `0`, the
#' loop will run until another stopping criteria is met.
#' @param verbose Whether to print progress updates to the terminal.
#' @param ... Extra parameters.
#'
#' @return A list with formatted and checked control parameters.
#' @export
nested_sampling.control <- function(num_points = 500,
                                    first_update = 2.0,
                                    update_interval = 0.6,
                                    max_iter = NA,
                                    max_call = NA,
                                    dlogz = 0.5,
                                    verbose = TRUE) {
  check_number_whole(num_points, min = 1)
  first_update <- if (is.integer(first_update)) {
    first_update
  } else {
    round(first_update * num_points)
  }
  check_number_whole(first_update, min = 0)
  update_interval <- if (is.integer(update_interval)) {
    update_interval
  } else {
    round(update_interval * num_points)
  }
  check_number_whole(update_interval, min = 1)
  if (is.na(max_iter) && is.na(max_call && is.na(dlogz))) {
    cli::cli_abort("At least one of `max_iter`, `max_call`, or `dlogz` must be specified.")
  }
  max_iter <- if (is.na(max_iter)) {
    .Machine$integer.max
  } else {
    check_number_whole(max_iter, min = 1)
    max_iter
  }
  max_call <- if (is.na(max_call)) {
    .Machine$integer.max
  } else {
    check_number_whole(max_call, min = 1)
    max_call
  }
  dlogz <- if (is.na(dlogz)) {
    0
  } else {
    check_number_decimal(dlogz, min = 0)
    dlogz
  }
  check_bool(verbose)
  list(
    num_points = num_points,
    first_update = first_update,
    update_interval = update_interval,
    max_iter = max_iter,
    max_call = max_call,
    dlogz = dlogz,
    verbose = verbose
  )
}
