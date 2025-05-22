# Case Test: Three-dimensional, highly correlated gaussian
library(distributional)

n_dim <- 3
mean <- c(-1, 0, 1)
cov <- diag(n_dim)
cov[cov == 0] <- 0.95
inv_cov <- solve(cov)
prior_win <- 10
expected_log_z <- n_dim * (-log(2 * prior_win))

logl_norm <- -0.5 * (log(2 * pi) * n_dim + log(det(cov)))
log_l <- function(x) {
  -0.5 * crossprod(x - mean, crossprod(inv_cov, x - mean)) + logl_norm
}

test_that("Gaussian case works with prior function", {
  skip_on_cran()
  options("cli.progress_show_after" = 60)
  set.seed(42)
  prior <- \(x) qunif(x, -10, 10)
  sampler <- nested_sampling(log_l, prior, names = c("x1", "x2", "x3"))
  expect_snapshot(sampler)

  compile(sampler)
  units <- sampler$get_live_points("unit")
  expect_equal(
    as.matrix(sampler$get_live_points()),
    t(apply(units, 1, prior))
  )

  expect_message(
    generate(sampler, max_iterations = 1000L, verbose = TRUE),
    "Reached Max Iterations: 1000/1000"
  )
  generate(sampler)
  expect_snapshot(sampler)
  result <- calculate(sampler)
  expect_lt(
    tail(result$log_evidence, 1) - expected_log_z,
    4 * tail(sqrt(result$log_evidence.var), 1)
  )
})

test_that("Gaussian case works with a vector of prior distributions", {
  skip_on_cran()
  set.seed(42)
  prior <- c(
    "x" = dist_uniform(-10, 10),
    "y" = dist_uniform(-10, 10),
    "z" = dist_uniform(-10, 10)
  )
  sampler <- nested_sampling(log_l, prior)
  expect_snapshot(sampler)

  compile(sampler)
  units <- sampler$get_live_points("unit")
  expect_equal(
    as.matrix(sampler$get_live_points()),
    t(apply(units, 1, \(x) qunif(x, -10, 10)))
  )

  generate(sampler)
  expect_snapshot(sampler)
  result <- calculate(sampler)
  expect_lt(
    tail(result$log_evidence, 1) - expected_log_z,
    4 * tail(sqrt(result$log_evidence.var), 1)
  )
})
