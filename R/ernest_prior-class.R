#' Specify a prior space for nested sampling
#'
#' @param x Either a function or a vector with S3 class `distribution`. See details.
#' @param variables Either a character vector of names for each free variable in the
#' parameter space, or a single number indicating the number of variables.
#' @param ... Arguments forwarded to `vctrs::vec_as_names`.
#'
#' @details
#' The `ernest_prior` function is used to specify a prior space for nested
#' sampling. It can take either a function or a vector with S3 class
#' `distribution`. If a function is provided, it should take in a
#' matrix slice from the unit space and return the slice in the original
#' parameter space. If a distribution vector is provided, it will be used to
#' create a function that applies the inverse probability transformation to
#' each slice.
#'
#' Note that, at this time, `ernest_prior` only supports the following families:
#' `dist_beta`, `dist_binomial`, `dist_cauchy`, `dist_chisq`, `dist_exponential`,
#' `dist_f`, `dist_gamma`, `dist_geometric`, `dist_hypergeometric`,
#' `dist_lognormal`, `dist_negbin`, `dist_normal`, `dist_poisson`,
#' `dist_student_t`, `dist_uniform`, and `dist_weibull`. You may also use
#' `dist_truncated` to specify a truncated distribution around one of the prior
#' families.
#'
#' @returns An `ernest_prior` object, which contains the following elements:
#'
#' @export
ernest_prior <- function(x, ...) {
  UseMethod("ernest_prior")
}

#' @rdname ernest_prior
#' @export
ernest_prior.function <- function(x, variables, ...) {
  if (is.numeric(variables)) {
    check_number_whole(variables, min = 1)
    variables <- vctrs::vec_as_names(rep("", variables), ...)
  }
  new_ernest_prior(
    variables = variables,
    fn = x,
    dist = NULL
  )
}

#' @rdname ernest_prior
#' @export
ernest_prior.distribution <- function(x, ...) {
  names <- vctrs::vec_names2(x, ...)
  x <- unname(x)
  x_data <- vctrs::vec_data(x)
  if (any(vapply(x_data, is.null, logical(1L)))) {
    abort("Some distributions are NULL.")
  }
  p <- length(x_data)
  expr_list <- do.call(
    mapply,
    c(ernest_prior, list(x_data, idx = seq_along(x_data)), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  )
  expr_vec <- call2("c", !!!expr_list, .ns = "base")
  fn <- new_function(
    exprs(x = ),
    expr(matrix(!!expr_vec, nrow = nrow(x), ncol = !!p))
  )
  new_ernest_prior(
    variables = names,
    fn = fn,
    dist = x
  )
}

#' @export
ernest_prior.dist_default <- function(x, idx, ...) {
  cli::cli_abort(
    "The distribution class {class(x)[[1]]} does not support `ernest_prior()`"
  )
}

#' @export
ernest_prior.dist_beta <- function(x, idx, ...) {
  call2("qbeta", p = expr(x[,!!idx]), x[["shape1"]], x[["shape2"]], .ns = "stats")
}

#' @export
ernest_prior.dist_binomial <- function(x, idx, ...) {
  call2("qbinom", p = expr(x[,!!idx]), x[["n"]], x[["p"]], .ns = "stats")
}

#' @export
ernest_prior.dist_cauchy <- function(x, idx, ...) {
  call2("qcauchy", p = expr(x[,!!idx]), x[["location"]], x[["scale"]], .ns = "stats")
}

#' @export
ernest_prior.dist_chisq <- function(x, idx, ...) {
  call2("qchisq", p = expr(x[,!!idx]), x[["df"]], .ns = "stats")
}

#' @export
ernest_prior.dist_exponential <- function(x, idx, ...) {
  call2("qexp", p = expr(x[,!!idx]), x[["rate"]], .ns = "stats")
}

#' @export
ernest_prior.dist_f <- function(x, idx, ...) {
  call2("qf", p = expr(x[,!!idx]), x[["df1"]], x[["df2"]], .ns = "stats")
}

#' @export
ernest_prior.dist_gamma <- function(x, idx, ...) {
  call2("qgamma", p = expr(x[,!!idx]), x[["shape"]], x[["rate"]], .ns = "stats")
}

#' @export
ernest_prior.dist_geometric <- function(x, idx, ...) {
  call2("qgeom", p = expr(x[,!!idx]), x[["p"]], .ns = "stats")
}

#' @export
ernest_prior.dist_hypergeometric <- function(x, idx, ...) {
  call2("qhyper", p = expr(x[,!!idx]), x[["m"]], x[["n"]], x[["k"]], .ns = "stats")
}

#' @export
ernest_prior.dist_lognormal <- function(x, idx, ...) {
  call2("qlnorm", p = expr(x[,!!idx]), x[["mu"]], x[["sigma"]], .ns = "stats")
}

#' @export
ernest_prior.dist_negbin <- function(x, idx, ...) {
  call2("qnbinom", p = expr(x[,!!idx]), x[["n"]], x[["p"]], .ns = "stats")
}

#' @export
ernest_prior.dist_normal <- function(x, idx, ...) {
  call2("qnorm", p = expr(x[,!!idx]), x[["mu"]], x[["sigma"]], .ns = "stats")
}

#' @export
ernest_prior.dist_poisson <- function(x, idx, ...) {
  call2("qpois", p = expr(x[,!!idx]), x[["l"]], .ns = "stats")
}

#' @export
ernest_prior.dist_student_t <- function(x, idx, ...) {
  qt_call <- if (rlang::is_empty(x[["ncp"]])) {
    call2("qt", p = expr(x[,!!idx]), x[["df"]], .ns = "stats")
  } else {
    call2("qt", p = expr(x[,!!idx]), x[["df"]], x[["ncp"]], .ns = "stats")
  }
  expr(!!qt_call * !!x[["sigma"]] + !!x[["mu"]])
}

#' @export
ernest_prior.dist_uniform <- function(x, idx, ...) {
  call2("qunif", p = expr(x[,!!idx]), x[["l"]], x[["u"]], .ns = "stats")
}

#' @export
ernest_prior.dist_weibull <- function(x, idx, ...) {
  call2("qweibull", p = expr(x[,!!idx]), x[["shape"]], x[["scale"]], .ns = "stats")
}

#' @importFrom distributional cdf
#' @export
ernest_prior.dist_truncated <- function(x, idx, ...) {
  F_lwr <- cdf(x[["dist"]], x[["lower"]])
  F_upr <- cdf(x[["dist"]], x[["upper"]])
  qt <- ernest_prior(x[["dist"]], idx)
  qt <- call_modify(qt, p = expr(!!F_lwr + x[,!!idx] * !!(F_upr - F_lwr)))
  expr(pmin(pmax(!!x[["lower"]], !!qt), !!x[["upper"]]))
}

#' Construct a new ernest prior object
#' @noRd
new_ernest_prior <- function(variables, fn, dist = NULL, check = FALSE) {
  check_character(variables)
  if (!is_empty(dist) && !inherits(dist, "distribution")) {
    stop_input_type(dist, "a distribution vector")
  }
  check_function(fn)
  check_bool(check)

  structure(
    list(
      "variables" = variables,
      "dist" = dist,
      "fn" = fn
    ),
    class = "ernest_prior"
  )
}

#' @export
format.ernest_prior <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_text("{.emph Prior Transformation}")
    cli::cli_text("Num. Variables: {length(x$variables)}")
    var_names <- x$variables[x$variables != ""]
    if (!is_empty(var_names)) {
      cli::cli_text("{var_names}")
    }
    num_unnamed <- sum(x$variables == "")
    if (num_unnamed > 0) {
      cli::cli_alert_info("Includes {num_unnamed} unnamed variables.")
    }
    if (!is_empty(x$dist)) {
      cli::cli_text("Distributions:")
      cli::cli_text("{x$dist}")
    } else {
      cli::cli_text("User provided prior transformation function.")
    }
  })
}

#' @export
print.ernest_prior <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
variables.ernest_prior <- function(x, ...) {
  x$variables
}

#' @export
nvariables.ernest_prior <- function(x, ...) {
  length(variables(x, ...))
}
