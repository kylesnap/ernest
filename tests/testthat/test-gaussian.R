# Case Test: Three-dimensional, highly correlated gaussian

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
prior <- create_uniform_prior(n_dim, lower = -10, upper = 10, varnames = c("x1", "x2", "x3"))

test_that("Gaussian case works with prior function", {
  skip_on_cran()
  options("cli.progress_show_after" = 60)
  set.seed(42)

  sampler <- nested_sampling(log_l, prior)
  expect_snapshot(sampler)

  compile(sampler)
  units <- sampler$live_points$unit
  expect_equal(
    apply(t(apply(units, 1, prior$fn)), 1, log_l),
    sampler$live_points$log_lik
  )

  result <- generate(sampler)
  expect_snapshot(result)
  sum <- summary(result)
  expect_snapshot(sum)
  expect_lt(
    abs(sum$log_evidence - expected_log_z),
    3 * sum$log_evidence_err
  )
})
