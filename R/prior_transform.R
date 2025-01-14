#' Define a prior transformation
#'
#' @param x A generic object.
#' @param ... Additional arguments that are all ignored.
#'
#' @return A prior_transform object that can be used by ernest.
#' @export
prior_transform <- function(x, ...) {
  UseMethod("prior_transform")
}

#' @rdname prior_transform
#' @export
prior_transform.default <- function(x, ...) {
  rlang::abort("The prior transformation must be either a distrubtion3 object or a valid R function.")
}

#' @rdname prior_transform
#'
#' @param distribution A distribution object from the `distrubtion3` package.
#' @param name A character string to name the prior transformation.
#'
#' @return A prior_transform object that can be used by ernest.
prior_transform.distribution <- function(distribution, name = NULL, ...) {
  fn <- function(q) quantile(distribution, q)
  support <- unname(distributions3::support(distribution))
  obj <- new_prior_transform(
    fn,
    name = name,
    description = paste(distribution),
    support = support
  )
  validate_prior_transform(obj)
}

#' @rdname prior_transform
#'
#' @param fn A function that maps values unit hypercube to the support of the
#' prior.
#' @param name A character string to name the prior transformation.
#' @param support A numeric vector of length 2 that defines the support of the
#' prior.
#'
#' @details If support is `NULL`, ernest will try to infer the support by
#' passing zero and one to `fn`.
#'
#' @export
prior_transform.function <- function(fn, name = NULL, support = NULL, ...) {
  support <- if (is.null(support)) {
    c(fn(0), fn(1))
  } else {
    unname(support)
  }
  obj <- new_prior_transform(
    fn,
    name = name,
    description = "User-provided function",
    support = support
  )
  validate_prior_transform(obj)
}

#' Constructor
new_prior_transform <- function(
    fn = NULL,
    name = NULL,
    description = NULL,
    support = c(-Inf, Inf)
  ) {
  structure(
    list(
      fn = fn,
      name = name,
      description = description,
      support = support
    ),
    class = "prior_transform"
  )
}

#' Validator
validate_prior_transform <- function(prior_transform) {
  if (!rlang::is_function(prior_transform$fn)) {
    stop("The `transform_function` must be a valid R function.")
  }
  if (!is.null(prior_transform$name) && !is.character(prior_transform$name)) {
    stop("The `name` must be a character string or NULL.")
  }
  if (!is.null(prior_transform$description) &&
    !is.character(prior_transform$description)) {
      stop("The `description` must be a character string or NULL.")
  }
  if (!is.numeric(prior_transform$support) ||
    length(prior_transform$support) != 2) {
      stop("The `support` must be a numeric vector of length 2.")
  }
  if (prior_transform$support[1] >= prior_transform$support[2]) {
    stop("The lower bound of the support must be less than the upper bound.")
  }
  prior_transform
}
