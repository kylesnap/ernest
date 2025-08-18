#' Example Nested Sampling Run with Ernest
#'
#' Load a precomputed example nested sampling run generated using the ernest
#' package. It demonstrates a typical output from a nested sampling run on a
#' simple 3-dimensional Gaussian likelihood, with a uniform prior over each
#' dimension. This dataset is intended for use in documentation, tutorials,
#' and gainining experience with `ernest_run`'s S3 methods.
#'
#' The likelihood used to generate the points is \eqn{MVN(0, \Sigma)}, with
#' each variance in \eqn{\Sigma} set to 1 and each covariance set to 0.95.
#' The prior for each parameter is uniform on the interval `[-10, 10\]`.
#'
#' @format An object of class `ernest_run` containing the results of a nested
#' sampling run.
#'
#' @details
#' This run uses the following non-default settings:
#' * `log_lik`: A 3D multivariate Gaussian with mean zero and covariance
#' matrix `diag(0.95, 3)`.
#' * `prior`: Uniform over each dimension (x, y, z) in the range [-10, 10].
#' Seed: 42
#'
#' View the `$spec` element of `example_run` to see the full R specification
#' of the likelihood and prior.
#'
#' @srrstats {G5.1} This allows us to run a fairly quick run for the examples in
#' the documentation. Users may want to use this function in their own
#' experimentation with ernest.
#'
#' @source This example problem comes from the crash course for the
#' [dynesty](https://dynesty.readthedocs.io/en/v2.1.5/crashcourse.html)
#' Python-based nested sampling software.
"example_run"
