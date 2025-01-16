# Test for new_sampler function
test_that("new_sampler creates a valid sampler object", {
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior <- function(x) qnorm(x)
  sampler <- new_sampler(log_lik = log_lik, prior = prior, num_dim = 2)

  expect_s3_class(sampler, "sampler")
  expect_equal(sampler$num_dim, 2)
  expect_true(is.function(sampler$log_lik))
  expect_true(is.function(sampler$prior))
})

# Test for validate function
test_that("validate_sampler checks for valid sampler object", {
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior <- function(x) qnorm(x)
  sampler <- new_sampler(log_lik = log_lik, prior = prior, num_dim = 2)

  expect_s3_class(validate_sampler(sampler), "sampler")

  invalid_sampler <- new_sampler(log_lik = log_lik, prior = prior, num_dim = -1)
  expect_error(validate_sampler(invalid_sampler), "num_dim must be a positive integer")

  invalid_sampler <- new_sampler(log_lik = "not a function", prior = prior, num_dim = 2)
  expect_error(validate_sampler(invalid_sampler), "log_lik must be a function or NULL")

  invalid_sampler <- new_sampler(log_lik = log_lik, prior = "not a function", num_dim = 2)
  expect_error(validate_sampler(invalid_sampler), "prior must be a function or NULL")
})

# Test for propose function
test_that("propose.sampler generates a valid point", {
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior <- function(x) qnorm(x)
  sampler <- new_sampler(log_lik = log_lik, prior = prior, num_dim = 2)

  proposal <- propose(sampler)

  expect_type(proposal, "list")
  expect_length(proposal$u_point, 2)
  expect_length(proposal$point, 2)
  expect_true(is.finite(proposal$log_lik))
})

# Test for lrps function
test_that("lrps.sampler throws an error", {
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior <- function(x) qnorm(x)
  sampler <- new_sampler(log_lik = log_lik, prior = prior, num_dim = 2)

  expect_error(lrps(sampler, u_point = runif(2), log_lik_criterion = -10),
               "The 'lrps' method should be overwritten by a subclass of sampler")
})

# Test for update function
test_that("update.sampler returns the same sampler", {
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior <- function(x) qnorm(x)
  sampler <- new_sampler(log_lik = log_lik, prior = prior, num_dim = 2)

  updated_sampler <- update_sampler(sampler, live_points = matrix(runif(4), ncol = 2))

  expect_identical(updated_sampler, sampler)
})
