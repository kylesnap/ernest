#' View, set, and count the variables in an `ernest_sampler` object
#'
#' Read and modify the variable names within an `ernest_sampler` object, or
#' count the variables (i.e., the dimensionality of the nested sampling problem.
#'
#' @param x An `ernest_sampler` object.
#' @param variables,value Either a character vector, an empty tibble of
#' variable names, or a scalar value describing the dimensions of the
#' parameter space.
#' @param ... Must be left empty.
#'
#' @returns For [variables()], a character vector of variable names. For
#' [variables<-()] and [set_variables()], the modified `ernest_sampler` object.
#' For [nvariables()], a scalar integer.
#'
#' @note The `ernest_sampler` stores an empty prototype tibble to keep track
#' of the variable names and the dimensionality of the sampled prior space.
#' Overwriting the number of parameters within an existing sampler is not supported.
#'
#' @importFrom posterior variables
#' @method variables ernest_sampler
#' @export
variables.ernest_sampler <- function(x, ...) {
  check_dots_empty()
  x$variables
}

#' @rdname variables.ernest_sampler
#'
#' @param variables,value Either a character vector or empty tibble of variable names,
#' or a scalar value describing the dimensions of the parameter space (in which
#' case, variable names will be denoted with `X...i`).
#'
#' @importFrom posterior variables<-
#' @method variables<- ernest_sampler
#' @export
`variables<-.ernest_sampler` <- function(x,..., value) {
  check_dots_empty()
  new_ptype <- make_ptype(value)
  x$variables <- new_ptype
  x
}

#' @rdname variables.ernest_sampler
#' @importFrom posterior set_variables
#' @method set_variables ernest_sampler
#' @export
set_variables.ernest_sampler <- function(x, variables, ...) {
  check_dots_empty()
  `variables<-`(x, variables)
}

#' @rdname variables.ernest_sampler
#' @importFrom posterior nvariables
#' @method nvariables ernest_sampler
#' @export
nvariables.ernest_sampler <- function(x, ...) {
  check_dots_empty()
  length(x$variables)
}

#' Transform `ernest_sampler` to `posterior::draws` objects
#'
#' Transform an `ernest_sampler` object to a format supported by the
#' posterior package.
#'
#' @param x An `ernest_sampler` object, or another \R object for which
#' the methods are defined.
#' @param scale One of `original` or `unit`. If `unit`, the returned
#' `draws` object will contain points expressed in the unit hypercube, which
#' was originally used by ernest for generated sampling. If `original` (the
#' default), the returned `draws` object will contain points in the original
#' parameter space defined by the prior transformation.
#' @param inc_live Should the live points be included? Defaults to `TRUE`.
#' @param ... Arguments passed to individual methods (if applicable).
#'
#' @rdname as_draws-ernest
#' @importFrom posterior as_draws
#' @method as_draws ernest_sampler
#' @export
as_draws.ernest_sampler <- function(x, scale = c("original", "unit"),
                                   inc_live = TRUE, ...) {
  as_draws_list(
    x, scale = scale, inc_live = inc_live
  )
}

#' @rdname as_draws-ernest
#' @importFrom posterior as_draws_list
#' @method as_draws_list ernest_sampler
#' @export
as_draws_list.ernest_sampler <- function(x,
                                        scale = c("original", "unit"),
                                        inc_live = TRUE,
                                        ...) {
  scale <- arg_match(scale)
  lst <- if (scale == "original") {
    "points"
  } else {
    "units"
  }
  if (x$n_iterations == 0) {
    cli::cli_abort("No iterations have been run with this sampler.")
  }
  dead_points <- x$dead_points[[lst]]
  live_points <- if (inc_live) {
    x$live_points[[lst]]
  } else {
    NULL
  }

  points <- rbind(dead_points, live_points)
  point_list <- lapply(seq_len(ncol(points)), function(i) points[,i])
  names(point_list) <- x$variables

  log_weight <- calculate(x)[["log_weight"]]
  if (is.null(log_weight)) {
    cli::cli_abort("No log_weight found in ernest_sampler object.")
  }
  if (!inc_live) {
    log_weight <- head(log_weight, x$n_iterations)
  }
  posterior::weight_draws(
    posterior::as_draws_list(point_list),
    weights = log_weight,
    log = TRUE
  )
}

#' @rdname as_draws-ernest
#' @importFrom posterior as_draws_matrix
#' @method as_draws_matrix ernest_sampler
#' @export
as_draws_matrix.ernest_sampler <- function(x, scale = c("original", "unit"), inc_live = TRUE, ...) {
  as_draws_matrix(as_draws_list.ernest_sampler(x, scale = scale, inc_live = inc_live,...))
}
