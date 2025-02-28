# An N-D Gaussian Distribution with Zero Mean and Given Correlation
make_gaussian <- function(num_dim, corr = 0.95) {
  mean <- seq(-1, 1, length.out = num_dim)
  sigma <- diag(num_dim)
  sigma[sigma == 0] <- 0.95

  list(
    log_lik = rlang::new_function(
      rlang::exprs(x = ),
      rlang::expr({
        dim(x) <- c(1, !!num_dim)
        mvtnorm::dmvnorm(
          x,
          mean = !!mean,
          sigma = !!sigma,
          log = TRUE
        )
      }),
    ),
    prior_transform = \(x) qunif(x, -10, 10)
  )
}

gaussian_2 <- make_gaussian(2)
