#' Code for preparing the example ernest_run object.
#'
#' @srrstats {G5.1} This allows us to run a fairly quick run for the examples in
#' the documentation. Users may want to use this function in their own
#' experimentation with ernest.

#' Log-likelihood of 3D Gaussian
#' @param theta a vector of parameter values
log_lik_mvn <- function(theta) {
  nvar <- 3
  sigma <- diag(0.95, nrow = 3) # Covariance matrix
  det_sigma <- log(det(sigma))
  prec <- solve(sigma) # Precision matrix (Sigma^-1)
  log_norm <- -0.5 * (log(2 * pi) * nvar + det_sigma) # Normalization for MVG

  drop(-0.5 * crossprod(theta, crossprod(prec, theta)) + log_norm)
}

run1 <- ernest_sampler(
  log_lik_mvn,
  create_uniform_prior(
    lower = -10,
    upper = 10,
    names = c("x", "y", "z")
  ),
  nlive = 100,
  seed = 24
) |>
  generate()
run2 <- ernest_sampler(
  log_lik_mvn,
  create_uniform_prior(
    lower = -10,
    upper = 10,
    names = c("x", "y", "z")
  ),
  nlive = 100,
  seed = 42
) |>
  generate()

merged <- merge(run1, run2)
merged

run3 <- generate(merged, min_logz = 0.01)
#usethis::use_data(example_run, overwrite = TRUE)
