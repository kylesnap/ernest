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
#' @return An `ernest_run` object containing the results of the nested
#' sampling run.
#'
#' @srrstats {G5.1} This allows us to run a fairly quick run for the examples in
#' the documentation. Users may want to use this function in their own
#' experimentation with ernest.
#'
#' @seealso [nested_sampling()] [ernest_run-class]
#' @examples
#' \dontrun{
#' # Load the example run
#' run_example()
#' }
#' @export
run_example <- function() {
  set.seed(42)
  sampler <- nested_sampling(
    log_lik = example_log_l(),
    prior = create_uniform_prior(
      3,
      lower = -10,
      upper = 10,
      varnames = c("X", "Y", "Z")
    ),
    n_points = 500
  )
  run <- generate(sampler)
  rm(sampler)
  run
}

#' Function factor for the Gaussian log-likelihood.
example_log_l <- function() {
  n_dim <- 3
  sigma <- diag(0.95, nrow = 3) # Covariance matrix
  det_sigma <- log(det(sigma))
  prec <- solve(sigma) # Precision matrix (Sigma^-1)
  log_norm <- -0.5 * (log(2 * pi) * n_dim + det_sigma) # Normalization for MVG

  # Log-likelihood of MVG(0, Sigma)
  function(theta) {
    drop(-0.5 * crossprod(theta, crossprod(prec, theta)) + log_norm)
  }
}
