#' Specify a prior space for nested sampling
#'
#' @param x A vector of class `distribution`, describing each variable in the
#' parameter space.
#' @param names A character vector of length equal to `x` or `NULL`.
#' @param repair Either a string or a function. See [vctrs::vec_as_names()] for
#' details on name repair.
#' @param ... Arguments forwarded to [vctrs::vec_as_names()].
#'
#' @returns An `ernest_prior` object, which contains the following elements:
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
                                      call = args$call,
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

#' Compile an ernest_prior into an inverse transformation function for NS
#'
#' @param x An `ernest_prior` object.
#' @param ... Ignored
#'
#' @returns A function that takes a vector of uniform random variables and returns
#' a vector of the same length with the corresponding values from the prior
#' distribution.
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
