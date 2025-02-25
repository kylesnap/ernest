#' Configure an ErnestSampler object
#'
#' Ernest conducts nested sampling in an enviroment bound to the nested sampler.
#' This function configures this environment so that it contains the necessary
#' variables.
#'
#' @param object An ErnestSampler or ErnestSampler object.
#' @param ... Ignored.
#'
#' @return `object`, now attached to an `ErnestWorkspace`
#' @rdname compile
compile.ErnestSampler <- function(object, ...) {
  object@wrk <- ErnestWorkspace$new(
    object@log_lik,
    object@prior_transform,
    object@n_dim,
    object@n_points
  )
  object
}

#' S7 dispatch method
#' @noRd
compile_ernest <- new_external_generic("generics", "compile", "object")
method(compile_ernest, ErnestSampler) <- compile.ErnestSampler
