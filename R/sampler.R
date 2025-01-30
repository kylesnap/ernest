#' Make a likelihood-restricted prior sampler
#'
#' This is the base class for creating a likelihood-based prior sampler. All
#' of ernest's samplers inherit from this superclass.
#'
#' @param log_lik A function that takes in one vector of parameters and returns
#' a scalar log-likelihood value.
#' @param prior_transform An object of class "prior_transform" used to transform
#' values in the unit hypercube values to the original parameter space.
#' @param num_dim An integer number of dimensions in the parameter space.
#' @param name A name for the given sampler.
#' @param description A short description of the sampler.
#' @param max_attempts The maximum number of attempts to find a valid point during
#' region-based sampling, including during uniform sampling.
#' @param ... Name-value pairs for additional elements of samplers that
#' subclass this sampler.
#' @param subclass The subclasses of this sampler.
#'
#' @returns A `sampler` object, used to propose new particles under a likelihood
#' constraint.
#'
#' @export
new_sampler <- function(log_lik = NULL, prior_transform = NULL, num_dim = 0,
                        name = "Sampler Superclass",
                        description = "Test Class",
                        max_attempts = 1e6,
                        ...,
                        subclass = character())  {
  check_function(log_lik, allow_null = TRUE)
  if (!is.null(prior_transform) && !inherits(prior_transform, "prior_transform")) {
    cli::cli_abort("`prior_transform` must be an object of class 'prior_transform'.")
  }
  check_number_whole(num_dim, min = 0)
  check_string(name)
  check_string(description)
  check_number_whole(max_attempts, min = 1)
  check_character(subclass)

  elems <- list(
    log_lik = log_lik,
    prior_transform = prior_transform,
    num_dim = num_dim,
    name = name,
    description = description,
    max_attempts = max_attempts
  )

  elems <- if (!rlang::is_empty(list2(...))) {
    check_unique_names(rlang::list2(...))
    c(elems, rlang::list2(...))
  } else {
    elems
  }
  structure(elems, class = c(subclass, "ernest_sampler"))
}

#' Refresh a sampler
#'
#' This runs the sampler through the method-specific `new_*_sampler()` function
#' to ensure that all of the elements of the sampler are still valid.
#'
#' @param sampler A sampler.
#'
#' @returns The `sampler` after a call to the corresponding constructor.
#'
#' @export
refresh_sampler <- function(sampler) {
  UseMethod("refresh_sampler")
}

#' @export
refresh_sampler.ernest_sampler <- function(sampler) {
  do.call(new_sampler, as.list(sampler))
}

#' Update a sampler object
#'
#' Updates the parameters within a sampler object. Unlike using `$`,
#' this function checks that the updated name already exists in the object,
#' and runs `refresh_sampler()` afterwards to check validity.
#'
#' @inheritParams refresh_sampler
#' @param ... Name-value pairs of _existing_ elements in `sampler` that should
#' be updated.
#'
#' @export
update_sampler <- function(sampler, ...) {
  if (!inherits(sampler, "ernest_sampler")) {
    cli::cli_abort("`sampler` must be an object of class 'ernest_sampler'.")
  }

  args <- rlang::list2(...)
  check_unique_names(args)

  names_new <- names(args)
  names_old <- names(sampler)
  names_exist <- names_new %in% names_old

  if (any(!names_exist)) {
    loc <- which(!names_exist)
    names <- names_new[loc]
    message <- c(
      "All elements of {.arg ...} must already exist.",
      i = "The following fields are new: {.str {names}}."
    )
    cli::cli_abort(message)
  }

  sampler <- update_sampler0(sampler, !!!args)
  refresh_sampler(sampler)
}

update_sampler0 <- function(sampler, ...) {
  # Performance variant only for internal use
  args <- list2(...)
  names <- names2(args)
  sampler[names] <- args
  sampler
}

#' Propose a new live point by randomly sampling within the unit hypercube.
#'
#' @param sampler The sampler object, inheriting from `ernest_sampler`
#' @param min_lik The log-likelihood criterion for the new particle.
#'
#' @returns A particle, which is a list with four components: "unit", "parameter",
#' "log_lik", and "num_calls". Returns NULL if the log-likelihood cannot be
#' overcome after a million tries.
#'
#' @export
propose_uniform <- function(sampler, min_lik) {
  UseMethod("propose_uniform")
}

#' @noRd
#' @export
propose_uniform.ernest_sampler <- function(sampler, min_lik) {
  propose_uniform_(
    sampler$log_lik, sampler$prior_transform$fn, sampler$num_dim,
    min_lik, sampler$max_attempts
  )
}

#' Propose a point by sampling from the bounded prior distribution.
#'
#' @param sampler The sampler object, inheriting from `ernest_sampler`
#' @param original The original position of the live point.
#' @param min_lik The log-likelihood criterion for the new particle.
#'
#' @returns A particle, which is a list with four components: "unit", "parameter",
#' "log_lik", and "num_calls". Returns NULL if the log-likelihood cannot be
#' overcome after a million tries.
#'
#' @export
propose_live <- function(sampler, original, min_lik) {
  UseMethod("propose_live")
}

#' @noRd
#' @export
propose_live.ernest_sampler <- function(sampler, original, min_lik) {
  cli::cli_warn("This sampler should be a subclass of `ernest_sampler`. Defaulting
                to unit-cube proposal.")
  propose_uniform(sampler, min_lik)
}

#' Update the bounds of a given sampler with the current distribution
#' of live points
#'
#' @param sampler The current live points
#' @param live The current live points
#'
#' @returns A sampler, with its bounding behaviour updated.
update_bounds <- function(sampler, live) {
  UseMethod("update_bounds")
}

#' @noRd
#' @export
update_bounds.ernest_sampler <- function(sampler, live) {
  sampler
}
