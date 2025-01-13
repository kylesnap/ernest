#' Likelihood-Restricted Prior Sampler
#'
#' The `sampler` class is a generic class for likelihood-restricted prior
#' samplers. It should never be called by nested.
#'
#' @param log_lik A function returning the log-likelihood
#' @param prior A function transforming the unit cube to the prior space
#' @param num_dim The number of dimensions
#' @noRd
new_sampler <- function(log_lik = NULL, prior = NULL,
                        name = "LRPS Internal Sampler",
                        num_dim = NULL, ..., subclass = character()) {
  structure(
    list(
      log_lik = log_lik,
      prior = prior,
      name = name,
      num_dim = as.integer(num_dim)
    ),
    class = c(subclass, "sampler")
  )
}

#' Refresh a sampler
refresh <- function(sampler) {
  UseMethod("refresh")
}

refresh.sampler <- function(sampler) {
  obj <- do.call(new_sampler, as.list(sampler))
  validate(obj)
}

#' Check an LRPS object for validity
#'
#' @noRd
validate <- function(object) {
  UseMethod("validate")
}

validate.default <- function(object) {
  rlang::abort("`validate` method not implemented for this object")
}

validate.sampler <- function(sampler) {
  if (sampler$num_dim <= 0) {
    rlang::abort("num_dim must be a positive integer")
  }
  if (
    !any(rlang::is_function(sampler$log_lik), rlang::is_null(sampler$log_lik))
  ) {
    rlang::abort("log_lik must be a function or NULL")
  }
  if (!any(rlang::is_function(sampler$prior), rlang::is_null(sampler$prior))) {
    rlang::abort("prior must be a function or NULL")
  }
  sampler
}

#' Propose a valid point in the unit-cube parameter space
#'
#' @param object An object of class `LRPS`
#'
#' @returns A particle list, containing the unit point, the point in the
#' original parameter space, and the log-likelihood
propose <- function(x) {
  UseMethod("propose")
}

propose.sampler <- function(sampler) {
  for (i in 1:1e7) {
    u_point <- runif(sampler$num_dim)
    point <- sampler$prior(u_point)
    log_lik <- sampler$log_lik(point)
    if (is.finite(log_lik)) {
      return(
        list(u_point = u_point, point = point, log_lik = log_lik)
      )
    }
  }
  rlang::abort("Failed to find a valid point after 1e7 iterations")
}

#' Likelihood restricted prior sampling
#'
#' @param object An object of class `sampler`
#' @param u_point The unit point that seeds the sampling process
#' @param log_lik_criterion The log-likelihood criterion
#' @param ... Additional arguments that are ignored
#'
#' @returns A particle list, containing the unit point, the point in the
#' original parameter space, and the log-likelihood
lrps <- function(object, u_point, log_lik_criterion, ...) {
  UseMethod("lrps")
}

lrps.sampler <- function(sampler, u_point, log_lik_criterion, ...) {
  rlang::abort(
    "The 'lrps' method should be overwritten by a subclass of sampler"
  )
}

#' Update the parameters of a given sampler
#'
#' @param sampler An object of class `sampler`
#' @param live_points A matrix, detailing the current live points within the
#' sampler
#'
#' @returns An updated sampler, with the new parameters.
#' @export
update <- function(sampler, live_points) {
  UseMethod("update")
}

update.default <- function(sampler, live_points) {
  rlang::abort("`refresh` method not implemented for this object")
}

update.sampler <- function(sampler, live_points) {
  sampler
}
