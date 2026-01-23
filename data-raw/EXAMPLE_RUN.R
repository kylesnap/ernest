#' Code for preparing the example ernest_run object.
#'
#' @srrstats {G5.1} This allows us to run a fairly quick run for the examples in
#' the documentation. Users may want to use this function in their own
#' experimentation with ernest.

#' Log-Likelihood of 3D Gaussian
#' @param theta a vector of parameter values
log_lik_mvn <- function(theta) {
  n_dim <- 3
  sigma <- diag(0.95, nrow = 3) # Covariance matrix
  det_sigma <- log(det(sigma))
  prec <- solve(sigma) # Precision matrix (Sigma^-1)
  log_norm <- -0.5 * (log(2 * pi) * n_dim + det_sigma) # Normalization for MVG

  drop(-0.5 * crossprod(theta, crossprod(prec, theta)) + log_norm)
}

sampler <- ernest_sampler(
  log_lik_mvn,
  create_uniform_prior(
    lower = -10,
    upper = 10,
    names = c("x", "y", "z")
  ),
  nlive = 1000,
  seed = 42
)

example_run <- generate(sampler)

usethis::use_data(example_run, overwrite = TRUE)
