#' View, set, and count the variables in an `ErnestSampler` object
#'
#' Read and modify the variable names within an `ErnestSampler` object, or
#' count the variables (i.e., the dimensionality of the nested sampling problem.
#'
#' @param x An `ErnestSampler` object.
#' @param variables,value Either a character vector, an empty tibble of
#' variable names, or a scalar value describing the dimensions of the
#' parameter space.
#' @param ... Must be left empty.
#'
#' @returns For [variables()], a character vector of variable names. For
#' [variables<-()] and [set_variables()], the modified `ErnestSampler` object.
#' For [nvariables()], a scalar integer.
#'
#' @note The `ErnestSampler` stores an empty prototype tibble to keep track
#' of the variable names and the dimensionality of the sampled prior space.
#' Overwriting the number of parameters within an existing sampler is not supported.
#'
#' @importFrom posterior variables
#' @method variables ErnestSampler
#' @export
variables.ErnestSampler <- function(x, ...) {
  check_dots_empty()
  names(x$ptype)
}

#' @rdname variables.ErnestSampler
#'
#' @param variables,value Either a character vector or empty tibble of variable names,
#' or a scalar value describing the dimensions of the parameter space (in which
#' case, variable names will be denoted with `X...i`).
#'
#' @importFrom posterior variables<-
#' @method variables<- ErnestSampler
#' @export
`variables<-.ErnestSampler` <- function(x,..., value) {
  check_dots_empty()
  new_ptype <- make_ptype(value)
  if (ncol(new_ptype) != ncol(x$ptype)) {
    cli::cli_abort("{.arg variables} is a different length to the prototype in {.arg x}.")
  }
  x$ptype <- new_ptype
  x
}

#' @rdname variables.ErnestSampler
#' @importFrom posterior set_variables
#' @method set_variables ErnestSampler
#' @export
set_variables.ErnestSampler <- function(x, variables, ...) {
  check_dots_empty()
  `variables<-`(x, variables)
}

#' @rdname variables.ErnestSampler
#' @importFrom posterior nvariables
#' @method nvariables ErnestSampler
#' @export
nvariables.ErnestSampler <- function(x, ...) {
  check_dots_empty()
  ncol(x$ptype)
}

#' Transform `ErnestSampler` to `posterior::draws` objects
#'
#' Transform an `ErnestSampler` object to a format supported by the
#' posterior package.
#'
#' @param x An `ErnestSampler` object, or another \R object for which
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
#' @method as_draws ErnestSampler
#' @export
as_draws.ErnestSampler <- function(x, scale = c("original", "unit"),
                                   inc_live = TRUE, ...) {
  as_draws_list(
    x, scale = scale, inc_live = inc_live
  )
}

#' @rdname as_draws-ernest
#' @importFrom posterior as_draws_list
#' @method as_draws_list ErnestSampler
#' @export
as_draws_list.ErnestSampler <- function(x,
                                        scale = c("original", "unit"),
                                        inc_live = TRUE,
                                        ...) {
  scale <- arg_match(scale)
  if (is.null(x$wrk) || x$wrk$n_iter == 0) {
    cli::cli_abort("No iterations have been run with this sampler.")
  }
  dead_points <- x$wrk$get_dead(scale == "unit")

  live_points <- if (inc_live) {
    if (scale == "unit") {
      x$wrk$live_units[order(x$wrk$live_lik), ]
    } else {
      x$wrk$live_points[order(x$wrk$live_lik), ]
    }
  } else {
    NULL
  }
  points <- rbind(dead_points, live_points)
  point_list <- lapply(seq_len(ncol(points)), function(i) points[,i])
  names(point_list) <- names(x$ptype)

  log_weight <- calculate(x)[["log_weight"]]
  if (is.null(log_weight)) {
    cli::cli_abort("No log_weight found in ErnestSampler object.")
  }
  if (!inc_live) {
    log_weight <- head(log_weight, x$wrk$n_iter)
  }
  posterior::weight_draws(
    posterior::as_draws_list(point_list),
    weights = log_weight,
    log = TRUE
  )
}

#' #' @rdname as_draws-ernest
#' #' @export
#' as_draws_matrix.ErnestSampler <- function(x, scale = c("original", "unit"),
#'                                           inc_live = TRUE, ...) {
#'
#' }
