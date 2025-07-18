#' Gaussian Shells From Dynesty
#' https://github.com/joshspeagle/dynesty/blob/master/demos/
gaussian_shell <- function(n_dim) {
  c_1 <- numeric(n_dim)
  c_2 <- numeric(n_dim)
  c_1[1] <- 3.5
  c_1[1] <- 3.5

  r <- 2
  w <- 0.1
  const <- log(1 / sqrt(2 * pi * w^2))

  # Single-shell log likelihood
  log_lik_shell <- function(theta, c) {
    d <- sqrt(sum((theta - c)^2))
    const - (d - r)^2 / (2 * w^2)
  }

  # log-likelihood of two shells
  function(theta) {
    matrixStats::logSumExp(c(
      log_lik_shell(theta, c_1),
      log_lik_shell(theta, c_2)
    ))
  }
}
