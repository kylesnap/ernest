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
  print(class(sampler))

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

  expect_type(result, "list")
  expect_named(result, c("dead", "live_u", "live_point", "live_lik", "num_call", "num_iter"))
  expect_type(result$dead, "list")
  expect_type(result$live_u, "double")
  expect_equal(dim(result$live_u), c(10, 2))
  expect_type(result$live_point, "double")
  expect_equal(dim(result$live_point), c(10, 2))
  expect_type(result$live_lik, "double")
  expect_vector(result$live_lik, ptype = double(), size = 10)
  expect_vector(result$num_call, double())
  expect_vector(result$num_iter, double())
})
