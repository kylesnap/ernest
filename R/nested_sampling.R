#' Build a Nested Sampler
#'
#' The top-level function constructs an instance of an `ernest_sampling`,
#' containing the necessary information for nested sampling.
#'
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
