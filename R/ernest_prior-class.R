#' Define a Prior Space for Nested Sampling
#'
#' Create an object representing the prior space for nested sampling, using
#' either a vector of distributions or a transformation function.
#'
#' @param x A vector of objects satisfying [distributional::is_distribution()],
#' or a function that maps the unit cube to the prior space.
#' @param names A character vector of variable names, with length equal to `x`, or `NULL`.
#' For a function, names must not be `NULL`.
#' @param repair A string or function specifying how to handle duplicate names.
#' See [vctrs::vec_as_names()] for details.
#' @param ... Additional arguments passed to [vctrs::vec_as_names()].
#'
#' @return An `ernest_prior` object containing the prior specification.
#'
#' @details
#' The `ernest_prior` object supports two types of inputs:
#' - A vector of distributions, where each element specifies the prior for a variable.
#' - A transformation function that maps points from the unit cube to the prior space.
#'
#' @note
#' See \link[stats]{Distributions} for distributions currently supported through
#' `distributional`. In addition, `dist_truncated` is current supported when wrapped
#' around a base R distribution. If you need to use a distribution that is not
#' supported, you can define a custom transformation function.
#'
#' @examples
#' # Using distributions
#' dist <- distributional::dist_normal(mu = 0, sigma = 1)
#' prior <- ernest_prior(dist, names = "x")
#'
#' # Using a transformation function
#' prior_fn <- function(u) qunif(u, -5, 5)
#' prior <- ernest_prior(prior_fn, names = c("x", "y"))
#'
#' @importFrom vctrs vec_names2 vec_as_names vec_set_names
#' @export
ernest_prior <- function(x, ...) {
  UseMethod("ernest_prior")
}

#' @rdname ernest_prior
#' @export
ernest_prior.function <- function(x,
                                  names,
                                  repair = c("unique", "universal", "check_unique", "unique_quiet", "universal_quiet"),
                                  ...) {
  args <- list2(...)
  check_character(names, call = args$call)
  fn <- as_function(x)
  if (length(rlang::fn_fmls(fn)) != 1) {
    cli::cli_abort("`x` must be a function of exactly one argument.")
  }

  empty_vec <- vec_set_names(
    rep(distributional::dist_missing(), length(names)),
    vec_as_names(names, repair = repair, ...)
  )
  new_ernest_prior(empty_vec, fn = fn)
}

#' @rdname ernest_prior
#' @export
ernest_prior.distribution <- function(x,
                                      names = vec_names(x),
                                      repair = c("unique", "universal",
                                                 "check_unique", "unique_quiet",
                                                 "universal_quiet"
                                                 ),
                                      ...) {
  repair <- arg_match(repair)
  if (is_missing(names) || is_empty(names)) {
    names <- vec_names2(x)
  }
  x <- vec_set_names(x, vec_as_names(names, repair = repair, ...))

  validate_distributions(x)
  new_ernest_prior(x)
}

#' @noRd
validate_distributions <- function(x) {
  accepted <- c('beta', 'binomial', 'cauchy', 'chisq', 'exponential', 'f',
                'gamma', 'geometric', 'hypergeometric', 'lognormal', 'negbin',
                'normal', 'poisson', 'student_t', 'uniform', 'weibull')

  if (any(vapply(vctrs::vec_data(x), is.null, logical(1L)))) {
    cli::cli_abort("Some distributions are NULL.")
  }

  families <- family(x)
  non_compat <- x[which(!(families %in% c(accepted, "truncated")))]
  if (!is_empty(non_compat)) {
    cli::cli_abort(
      "The following distributions are not currently supported: {non_compat}."
    )
  }

  if (any(families == "truncated")) {
    truncs <- x[which(families == "truncated")]
    non_compat <- truncs[which(
      !(vapply(vctrs::vec_data(truncs), \(x) family(x[["dist"]]), character(1L)) %in% accepted))
    ]
    if (!is_empty(non_compat)) {
      cli::cli_abort(
        "The following truncated distributions are not currently supported: {non_compat}."
      )
    }
  }

  return(NULL)
}

new_ernest_prior <- function(vec, fn = NULL) {
  structure(
    vec,
    class = c("ernest_prior", class(vec)),
    fn = fn
  )
}

#' @noRd
#' @export
format.ernest_prior <- function(x, ...) {
  check_dots_empty()
  cli::cli_format_method({
    cli::cli_h3("Prior with {nvariables(x)} Variables")
    cli::cli_text("Names: {vctrs::vec_names(x)}")
  })
}

#' @noRd
#' @export
print.ernest_prior <- function(x, ...) {
  check_dots_empty()
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Compile an `ernest_prior` Object
#'
#' @description
#' Converts an `ernest_prior` object into an inverse transformation function
#' for use in nested sampling.
#'
#' @param object An `ernest_prior` object.
#' @param ... Additional arguments (ignored).
#'
#' @return A function that takes a vector of uniform random variables and
#' returns a vector of corresponding values from the prior distribution.
#'
#' @examples
#' dist <- distributional::dist_normal(mu = 0, sigma = 1)
#' prior <- ernest_prior(dist, names = "x")
#' fn <- compile(prior)
#' fn(c(0.1, 0.5, 0.9))
#'
#' @export
compile.ernest_prior <- function(object, ...) {
  if (!is_empty(attr(object, "fn"))) {
    return(attr(object, "fn"))
  }
  x_data <- vctrs::vec_data(object)

  expr_list <- do.call(
    mapply,
    c(compile, list(x_data, idx = seq_along(x_data)), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  )
  expr_vec <- call2("c", !!!expr_list, .ns = "base")
  new_function(
    exprs(x = ),
    expr(c(!!expr_vec))
  )
}

# ----- Compile functions for supported distributions
utils::globalVariables("x")

#' @export
compile.dist_default <- function(object, idx, ...) {
  cli::cli_abort(
    "The distribution class {class(x)[[1]]} does not support `ernest_prior()`"
  )
}

#' @export
compile.dist_beta <- function(object, idx, ...) {
  call2("qbeta", p = expr(x[!!idx]), object[["shape1"]], object[["shape2"]], .ns = "stats")
}

#' @export
compile.dist_binomial <- function(object, idx, ...) {
  call2("qbinom", p = expr(x[!!idx]), object[["n"]], object[["p"]], .ns = "stats")
}

#' @export
compile.dist_cauchy <- function(object, idx, ...) {
  call2("qcauchy", p = expr(x[!!idx]), object[["location"]], object[["scale"]], .ns = "stats")
}

#' @export
compile.dist_chisq <- function(object, idx, ...) {
  call2("qchisq", p = expr(x[!!idx]), object[["df"]], .ns = "stats")
}

#' @export
compile.dist_exponential <- function(object, idx, ...) {
  call2("qexp", p = expr(x[!!idx]), object[["rate"]], .ns = "stats")
}

#' @export
compile.dist_f <- function(object, idx, ...) {
  call2("qf", p = expr(x[!!idx]), object[["df1"]], object[["df2"]], .ns = "stats")
}

#' @export
compile.dist_gamma <- function(object, idx, ...) {
  call2("qgamma", p = expr(x[!!idx]), object[["shape"]], object[["rate"]], .ns = "stats")
}

#' @export
compile.dist_geometric <- function(object, idx, ...) {
  call2("qgeom", p = expr(x[!!idx]), object[["p"]], .ns = "stats")
}

#' @export
compile.dist_hypergeometric <- function(object, idx, ...) {
  call2("qhyper", p = expr(x[!!idx]), object[["m"]], object[["n"]], object[["k"]], .ns = "stats")
}

#' @export
compile.dist_lognormal <- function(object, idx, ...) {
  call2("qlnorm", p = expr(x[!!idx]), object[["mu"]], object[["sigma"]], .ns = "stats")
}

#' @export
compile.dist_negbin <- function(object, idx, ...) {
  call2("qnbinom", p = expr(x[!!idx]), object[["n"]], object[["p"]], .ns = "stats")
}

#' @export
compile.dist_normal <- function(object, idx, ...) {
  call2("qnorm", p = expr(x[!!idx]), object[["mu"]], object[["sigma"]], .ns = "stats")
}

#' @export
compile.dist_poisson <- function(object, idx, ...) {
  call2("qpois", p = expr(x[!!idx]), object[["l"]], .ns = "stats")
}

#' @export
compile.dist_student_t <- function(object, idx, ...) {
  qt_call <- if (rlang::is_empty(object[["ncp"]])) {
    call2("qt", p = expr(x[!!idx]), object[["df"]], .ns = "stats")
  } else {
    call2("qt", p = expr(x[!!idx]), object[["df"]], object[["ncp"]], .ns = "stats")
  }
  expr(!!qt_call * !!object[["sigma"]] + !!object[["mu"]])
}

#' @export
compile.dist_uniform <- function(object, idx, ...) {
  call2("qunif", p = expr(x[!!idx]), object[["l"]], object[["u"]], .ns = "stats")
}

#' @export
compile.dist_weibull <- function(object, idx, ...) {
  call2("qweibull", p = expr(x[!!idx]), object[["shape"]], object[["scale"]], .ns = "stats")
}

#' @importFrom distributional cdf
#' @export
compile.dist_truncated <- function(object, idx, ...) {
  F_lwr <- cdf(object[["dist"]], object[["lower"]])
  F_upr <- cdf(object[["dist"]], object[["upper"]])
  qt <- compile(object[["dist"]], idx)
  qt <- call_modify(qt, p = expr(!!F_lwr + x[!!idx] * !!(F_upr - F_lwr)))
  expr(pmin(pmax(!!object[["lower"]], !!qt), !!object[["upper"]]))
}
