test_that("unit_cube constructor works correctly", {
  num_dim <- 3
  sampler <- unit_cube(num_dim)
  expect_s3_class(sampler, "unit_cube")
  expect_equal(sampler$num_dim, num_dim)
})

test_that("new_unit_cube creates a valid sampler object", {
  num_dim <- 3
  sampler <- new_unit_cube(num_dim = num_dim)
  expect_s3_class(sampler, "unit_cube")
  expect_equal(sampler$num_dim, num_dim)
})

test_that("refresh.unit_cube refreshes the sampler correctly", {
  num_dim <- 3
  sampler <- unit_cube(num_dim)
  refreshed_sampler <- refresh(sampler)
  expect_s3_class(refreshed_sampler, "unit_cube")
  expect_equal(refreshed_sampler$num_dim, num_dim)
})

test_that("validate.unit_cube calls the NextMethod correctly", {
  num_dim <- 3
  sampler <- unit_cube(num_dim)
  expect_silent(validate(sampler))
})

test_that("lrps.unit_cube returns a valid point", {
  num_dim <- 3
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior <- function(u) qnorm(u)
  sampler <- unit_cube(num_dim = num_dim)
  sampler$log_lik <- log_lik
  sampler$prior <- prior
  sampler <- refresh(sampler)
  u_point <- runif(num_dim)
  log_lik_criterion <- -10
  result <- lrps.unit_cube(sampler, u_point, log_lik_criterion)
  expect_true(is.list(result))
  expect_true(is.numeric(result$u_point))
  expect_true(is.numeric(result$point))
  expect_true(is.finite(result$log_lik))
  expect_true(result$log_lik > log_lik_criterion)
})
