#' Construct a default prior distribution for a model
#'
#' This function generates a default prior distribution for the parameters of a
#' given model. Currently, it supports models created by [glm()].
#'
#' @param object A fitted model object, such as one from [glm()].
#' @param ... Additional arguments passed to methods.
#'
#' @return A vector with class `distributional`, describing the prior
#' distributions for the model coefficients, named accordingly.
#'
#' @details The default priors are based on the model type and family:
#' * For GLM models, coefficients are assigned a standard normal prior.
#' * For GLM models with Gaussian or Gamma families, a dispersion parameter is
#' assigned a truncated $Cauchy(0, 25)$ prior.
#'
#' @export
default_prior <- function(object, ...) {
  UseMethod("default_prior")
}

#' @rdname default_prior
#' @export
default_prior.glm <- function(object, ...) {
  var_labels <- c(
    if (attr(object$terms, "intercept")) ".intercept" else NULL,
    attr(object$terms, "term.labels")
  )
  n_var <- length(var_labels)
  dists <- vctrs::vec_set_names(
    rep(distributional::dist_normal(0, 1), n_var),
    var_labels
  )

  if (object$family$family %in% c("gaussian", "Gamma")) {
    c(
      dists,
      ".dispersion" = distributional::dist_truncated(
        distributional::dist_cauchy(0, 25),
        lower = 0
      )
    )
  } else {
    dists
  }
}
