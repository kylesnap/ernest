# A completely flat likelihood and prior distribution
make_flat <- function(num_dim) {
  list(
    log_lik = function(x) 0,
    prior_transform = prior_transform(\(x) x, num_dim, LETTERS[1:num_dim])
  )
}

# An N-D Gaussian Distribution with Zero Mean and Given Correlation
make_gaussian <- function(num_dim, corr = 0.95) {
  mean <- seq(-1, 1, length.out = num_dim)
  sigma <- diag(rep(1, length.out = num_dim))
  sigma[sigma == 0] <- corr
  precision <- solve(sigma)
  log_norm <- -0.5 * (log(2 * pi) * num_dim + log(det(sigma)))
  logz_truth <- num_dim * -log(2 * 10)

  list(
    log_lik = function(x) -0.5 *
      as.double((x - mean) %*% precision %*% (x - mean)) + log_norm,
    prior_transform = prior_transform(
      \(x) qunif(x, -10, 10),
      num_dim,
      LETTERS[1:num_dim]
    ),
    logz_truth = logz_truth
  )
}
