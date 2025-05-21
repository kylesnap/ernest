#' Get Variable Names from an `ernest_prior` object
#'
#' Get variable names from `ernest` objects.
#'
#' @param x An `ernest_prior` or `ernest_sampler` object.
#' @param ... Must be left empty.
#'
#' @returns `variables()` returns a vector of all variable names. `nvariables()`
#' returns the number of variables.
#'
#' @rdname variables-ernest
#' @importFrom vctrs vec_names
#' @importFrom posterior variables
#' @method variables ernest_prior
#' @export
variables.ernest_prior <- function(x, ...) {
  check_dots_empty()
  vec_names(x)
}

#' @rdname variables-ernest
#' @importFrom posterior variables
#' @method variables ernest_sampler
#' @export
variables.ernest_sampler <- function(x, ...) {
  check_dots_empty()
  x$variables
}

#' @rdname variables-ernest
#' @importFrom posterior nvariables
#' @method nvariables ernest_prior
#' @export
nvariables.ernest_prior <- function(x, ...) {
  check_dots_empty()
  length(vec_names(x))
}

#' @rdname variables-ernest
#' @importFrom posterior nvariables
#' @method nvariables ernest_sampler
#' @export
nvariables.ernest_sampler <- function(x, ...) {
  check_dots_empty()
  length(x$variables)
}

#' Set Variable Names in an `ernest_prior` Object
#'
#' @param x An `ernest_prior` or `ernest_sampler` object.
#' @param ... Must be left empty.
#'
#' @rdname variables_set-ernest
#' @importFrom vctrs vec_names
#' @importFrom posterior variables<-
#' @method variables<- ernest_prior
#' @export
`variables<-.ernest_prior` <- function(x, ..., value) {
  check_dots_empty()
  check_unique_names(value)
  x <- vec_set_names(x, value)
  x
}

#' @rdname variables_set-ernest
#' @importFrom posterior variables<-
#' @method variables<- ernest_sampler
#' @export
`variables<-.ernest_sampler` <- function(x, ..., value) {
  check_dots_empty()
  x$variables <- value
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
#' @param ... Arguments passed to individual methods (if applicable).
#'
#' @rdname as_draws-ernest
#' @importFrom posterior as_draws
#' @method as_draws ernest_sampler
#' @export
as_draws.ernest_sampler <- function(x, scale = c("original", "unit"), ...) {
  as_draws_list(x, scale = scale)
}

#' @rdname as_draws-ernest
#' @importFrom posterior as_draws_list
#' @method as_draws_list ernest_sampler
#' @export
as_draws_list.ernest_sampler <- function(x,
                                        scale = c("original", "unit"),
                                        ...) {
  scale <- arg_match(scale)
  if (x$niterations < 1L) {
    cli::cli_abort("No iterations have been run with this sampler.")
  }
  live <- x$get_live_points(scale, reorder = TRUE)
  dead <- x$get_dead_points(scale)

  points <- rbind(dead, live)
  point_list <- vctrs::df_list(!!!points)

  log_weight <- x$summary()$log_importance_weight
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
as_draws_matrix.ernest_sampler <- function(x, scale = c("original", "unit"), ...) {
  as_draws_matrix(as_draws_list.ernest_sampler(x, scale = scale,...))
}
