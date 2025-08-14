# An N-D Gaussian Distribution with Zero Mean and Given Correlation
library(LaplacesDemon)

make_gaussian <- function(num_dim, corr = 0.95) {
  mu <- seq(-1, 1, length.out = num_dim)
  Sigma <- diag(corr, nrow = num_dim)
  log_lik <- create_likelihood(
    LaplacesDemon::dmvn,
    mu = !!seq(-1, 1, length.out = num_dim),
    Sigma = Sigma,
    log = TRUE
  )
  prior <- create_uniform_prior(
    n_dim = num_dim,
    lower = -5,
    upper = 5
  )

  list(
    log_lik = log_lik,
    prior = prior
  )
}
