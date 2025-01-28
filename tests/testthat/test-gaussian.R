test_that("2D Gaussian Likelihood", {
  sigma <- 0.1
  mu1 <- rep(1, 2)
  mu2 <- rep(-1, 2)
  sigma_inv <- diag(2) / sigma^2

  log_lik <- function(x) {
    dx1 <- x - mu1
    dx2 <- x - mu2
    log1 <- -sum(dx1 %*% sigma_inv %*% dx1) / 2.0
    log2 <- -sum(dx2 %*% sigma_inv %*% dx2) / 2.0
  }

  result <- nested_sampling(
    log_lik,
    prior = prior_transform(
      "X" = distributional::dist_uniform(-10, 10),
      "Y" = distributional::dist_uniform(-10, 10)
    ),
    max_iter = 1000,
    verbose = TRUE
  )
  result |> print()
  tail(result$progress) |> print()
  result |> summary() |> print()
})
