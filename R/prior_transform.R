#' Specify a prior transformation for nested sampling
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named probability distributions
#' describing the marginal priors for each parameter.
#' @param fn A function that takes a vector of parameters in the unit hypercube and
#' returns a vector of parameters in the prior space.
#' @param num_dim The number of dimensions of the prior space.
#' @param .names A character vector of parameter names. Must be provided if
#' `x` is a function
#'
#' @return A `prior_transform` object.
#'
#' @export
prior_transform <- function(...) {
  UseMethod("prior_transform")
}

#' @rdname prior_transform
#' @export
prior_transform.distribution <- function(..., .names = NULL) {
  dots <- rlang::list2(...)
  dists <- list_c(dots)
  fn <- new_function(
    rlang::exprs(p = ),
    rlang::expr({
      lst <- stats::quantile(dists, p)
      vapply(seq_along(p), function(i) lst[[i]][i], FUN.VALUE = double(1))
    })
  )
  names <- if (is.null(.names)) {
    names2(dots)
  } else {
    .names
  }
  new_prior_transform(
    fn = fn,
    dim = length(dots),
    names = names2(dots),
    distribution = unname(dists),
    lb = stats::quantile(dists, 0),
    ub = stats::quantile(dists, 1)
  )
}

#' @rdname prior_transform
#' @export
prior_transform.function <- function(fn, num_dim, .names, ...) {
  new_prior_transform(
    fn = fn,
    dim = num_dim,
    names = .names,
    lb = rep(NA, num_dim),
    ub = rep(NA, num_dim)
  )
}

#' Constructor
new_prior_transform <- function(fn = NULL, dim = NULL,
                                names = NULL,
                                distribution = NULL,
                                lb = NULL, ub = NULL) {
  if (length(names) != dim) {
    cli::cli_abort("Length of names must match the number of dimensions")
  }
  names <- make.names(names, unique = TRUE)
  structure(
    list(
      "fn" = fn,
      "dim" = dim,
      "names" = names,
      "distributions" = distribution,
      "lb" = lb,
      "ub" = ub
    ),
    class = "prior_transform"
  )
}
