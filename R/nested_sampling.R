#' Build a Nested Sampler
#'
#' Constructs an instance of an `ernest_sampler` containing the necessary
#' information for performing nested sampling.
#'
#' @param x A function or a `glm` object. For functions, this represents the
#'   log-likelihood function. For `glm` objects, the model is used to derive
#'   the log-likelihood.
#' @param prior A prior distribution function or object. For `glm` objects,
#'   defaults to [default_prior()].
#' @param names Optional character vector of variable names.
#' @param sampler An [lrps_call()] object, declaring which likelihood-restricted prior 
#' sampler to use for the nested sampling run.
#' @param n_points The number of live points to use in the nested sampling run.
#' @param first_update The number of calls to the likelihood function
#'   before the first update of the sampler. If left as a double, this will be
#'   multiplied by the number of live points before being coerced to an
#'   integer.
#' @param update_interval The number of calls to the likelihood function
#'   between subsequent updates of the sampler. If left as a double, this will be
#'   multiplied by the number of live points before being coerced to an
#'   integer.
#' @param ... Additional arguments passed to methods.
#'
#' @return An `ernest_sampler` object.
#' @export
nested_sampling <- function(x, ...) {
  UseMethod("nested_sampling")
}

#' @rdname nested_sampling
#' @export
nested_sampling.function <- function(x,
                                     prior,
                                     names = NULL,
                                     sampler = rwmh_cube(),
                                     n_points = 500,
                                     first_update = 2.5,
                                     update_interval = 1.5,
                                     ...) {
  loglik <- ernest_likelihood(x)
  prior <- ernest_prior(
    prior,
    names = names,
    call = caller_env()
  )
  ernest_sampler$new(
    loglik,
    prior,
    sampler,
    n_points,
    first_update,
    update_interval
  )
}

#' @rdname nested_sampling
#' @export
nested_sampling.glm <- function(x,
                                prior = default_prior(x),
                                names = NULL,
                                sampler = rwmh_cube(),
                                n_points = 500,
                                first_update = 2.5,
                                update_interval = 1.5,
                                ...) {
  loglik <- ernest_likelihood(x)
  n_var <- length(x$coefficients) + if (x$family$family %in% c("gaussian", "Gamma")) 1L else 0L
  prior <- ernest_prior(prior, names)
  if (nvariables(prior) != n_var) {
    cli::cli_abort(
      "The model contains {n_var} variables, but the prior has {nvariables(prior)} variables."
    )
  }

  ernest_sampler$new(
    loglik,
    prior,
    sampler,
    n_points,
    first_update,
    update_interval
  )
}
