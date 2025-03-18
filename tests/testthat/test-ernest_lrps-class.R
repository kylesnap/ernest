test_that("new_ernest_lrps creates a valid object", {
  log_lik <- gaussian_2$log_lik
  prior_transform <- gaussian_2$prior_transform
  n_dim <- 2
  calls_between_updates <- 10

  sampler <- new_ernest_lrps(
    log_lik = log_lik,
    prior_transform = prior_transform,
    n_dim = n_dim,
    calls_between_updates = calls_between_updates
  )

  expect_s3_class(sampler, "ernest_lrps")
  expect_equal(sampler$log_lik, log_lik)
  expect_equal(sampler$prior_transform, prior_transform)
  expect_equal(sampler$n_dim, n_dim)
  expect_equal(sampler$calls_between_updates, calls_between_updates)
})

# Test for validate_ernest_lrps
test_that("validate_ernest_lrps validates the object correctly", {
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior_transform <- function(x) qnorm(x)
  n_dim <- 3
  calls_between_updates <- 10

  sampler <- new_ernest_lrps(
    log_lik = log_lik,
    prior_transform = prior_transform,
    n_dim = n_dim,
    calls_between_updates = calls_between_updates
  )

  validated_sampler <- validate_ernest_lrps(sampler)
  expect_equal(validated_sampler, sampler)
})

# Test for new_uniform_cube
test_that("new_uniform_cube creates a valid object", {
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior_transform <- function(x) qnorm(x)
  n_dim <- 3

  sampler <- new_uniform_cube(
    log_lik = log_lik,
    prior_transform = prior_transform,
    n_dim = n_dim
  )

  expect_s3_class(sampler, "uniform_cube")
  expect_equal(sampler$log_lik, log_lik)
  expect_equal(sampler$prior_transform, prior_transform)
  expect_equal(sampler$n_dim, n_dim)
})

# Test for new_rwmh_cube
test_that("new_rwmh_cube creates a valid object", {
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior_transform <- function(x) qnorm(x)
  n_dim <- 3
  update_interval <- 10
  num_steps <- 5
  target_acceptance <- 0.25
  epsilon <- 0.1

  sampler <- new_rwmh_cube(
    log_lik = log_lik,
    prior_transform = prior_transform,
    n_dim = n_dim,
    update_interval = update_interval,
    num_steps = num_steps,
    target_acceptance = target_acceptance,
    epsilon = epsilon
  )

  expect_s3_class(sampler, "rw_cube")
  expect_equal(sampler$log_lik, log_lik)
  expect_equal(sampler$prior_transform, prior_transform)
  expect_equal(sampler$n_dim, n_dim)
  expect_equal(sampler$update_interval, update_interval)
  expect_equal(sampler$num_steps, num_steps)
  expect_equal(sampler$epsilon[[1]], epsilon)
})
