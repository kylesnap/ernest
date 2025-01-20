#' Specify a prior transformation for nested sampling
#'
#' @param x Either a distribution3 distribution, a function, or a prior_transform object.
#' @param names A vector of character strings, naming each transformed variable.
#' @param lb,ub If 'x' is a function, the lower and upper bound for the
#' transformed variable, respectively.
#' @param ... If 'x' is a prior transform object, additional prior_transform
#' objects to combine together. Else, these dots should be empty.
#'
#' @returns A prior_transform object.
#' @export
set_prior_transform <- function(x, ...) {
  UseMethod("set_prior_transform")
}

#' @rdname set_prior_transform
#' @export
set_prior_transform.distribution <- function(x, names, ...) {
  fn <- \(p) stats::quantile(x, probs = p)
  sup <- unname(distributions3::support(x))
  obj <- new_prior_transform(
    fn = fn,
    dim = length(names),
    names = names,
    description = rep(as.character(x), length(names)),
    lb = rep(sup[1], length(names)),
    ub = rep(sup[2], length(names))
  )
  validate_prior_transform(obj)
}

#' @rdname set_prior_transform
#' @export
set_prior_transform.function <- function(x, names, lb = -Inf, ub = Inf, ...) {
  obj <- new_prior_transform(
    fn = x,
    dim = length(names),
    names = names,
    description = rep("user function", length(names)),
    lb = lb,
    ub = ub
  )
  validate_prior_transform(obj)
}

#' @rdname set_prior_transform
#' @export
set_prior_transform.prior_transform <- function(...) {
  priors <- list2(...)
  # Check that every object in priors inherits "prior_transform"
  if (any(map_lgl(priors, \(x) !inherits(x, "prior_transform")))) {
    stop("All arguments must be prior_transform objects.")
  }
  dim <- list_c(map_int(priors, \(x) x$dim))
  fn <- seq_func(map(priors, \(x) x$fn), dim)
  obj <- new_prior_transform(
    fn = fn,
    dim = dim,
    names = unlist(map(priors, \(x) x$name)),
    description = unlist(map(priors, \(x) x$description)),
    lb = unlist(map(priors, \(x) x$lb)),
    ub = unlist(map(priors, \(x) x$ub))
  )
  validate_prior_transform(obj)
}

#' @rdname set_prior_transform
#' @export
set_prior_transform.list <- function(x) {
  set_prior_transform.prior_transform(!!!x)
}

#' Constructor
new_prior_transform <- function(fn = NULL, dim = NULL,
                                names = NULL, description = NULL,
                                lb = NULL, ub = NULL) {
  structure(
    list(
      "fn" = fn,
      "dim" = dim,
      "names" = names,
      "description" = description,
      "lb" = lb,
      "ub" = ub
    ),
    class = "prior_transform"
  )
}

validate_prior_transform <- function(obj) {
  obj
}
