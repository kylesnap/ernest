#' Construct a default prior distribution for a given model
#'
#' @param object A fitted model object from [glm()].
#' @param ...
#'
#' @return A vector with class `distributional`, describing each prior placed
#' on the model coefficients, and named accordingly.
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
        dist_cauchy(0, 25),
        lower = 0
      )
    )
  } else {
    dists
  }
}
