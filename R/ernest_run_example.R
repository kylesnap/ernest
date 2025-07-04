#' Example Nested Sampling Run with Ernest
#'
#' This dataset contains the results of a nested sampling run performed on a
#' three-dimensional Gaussian likelihood with independent uniform priors on
#' each parameter.
#'
#' The likelihood used to generate the points is \eqn{MVN(0, \Sigma)}, with
#' each variance in \eqn{\Sigma} set to 1 and each covariance set to 0.95.
#' The prior for each parameter is uniform on the interval `[-10, 10\]`.
#'
#' @format An \code{ernest_run} object containing the results of the nested
#' sampling run. Users can build the run from scratch using
#' \code{vignette("nested-sampling-with-ernest", package = "ernest")}.
#' @usage data(ernest_run_example)
#' @seealso [nested_sampling()] [ernest_run-class]
"ernest_run_example"
