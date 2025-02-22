#' Configure an ErnestLRPS or ErnestRun object
#'
#' Ernest conducts nested sampling in an enviroment bound to the nested sampler.
#' This function configures this environment so that it contains the necessary
#' variables.
#'
#' @param x An ErnestLRPS or ErnestRun object.
#' @param overwrite If `x` is an ErnestRun object, whether the existing list
#' of `dead` points be preserved or erased.
#' @param ... Ignored.
#'
#' @return A new ErnestLRPS, with its `@wrk` property bound to a child enviroment
#' of `rlang::empty_env()`, containing the following.
NULL

#' @rdname compile
#' @export
method(compile, ErnestLRPS) <- function(x, ...) {
  x@wrk <- ErnestWorkspace$new(x@log_lik, x@prior_transform, x@n_dim, x@n_points)
  x
}
