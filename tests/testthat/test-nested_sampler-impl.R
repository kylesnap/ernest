library(testthat)

test_that("log_add_exp returns correct values", {
  expect_equal(log_add_exp(log(2), log(3)), log(5))
  expect_equal(log_add_exp(log(0.1), log(0.2)), log(0.3))
  expect_equal(log_add_exp(log(1), log(1)), log(2))
})

test_that("nested_sampling_impl returns expected structure", {
  sampler <- unit_cube(2)
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior <- function(u) qnorm(u)

  sampler$log_lik <- log_lik
  sampler$prior <- prior
  sampler <- refresh(sampler)

  result <- nested_sampling_impl(
    sampler = sampler,
    live_size = 10,
    dlogz = 0.01,
    max_iter = 100,
    max_call = 1000,
    first_update = 20L,
    update_iter = 10,
    verbose = FALSE
  )

  # TODO: Actual tests
})
