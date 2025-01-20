test_that("new_sampler creates a valid sampler object", {
  log_lik <- function(params) sum(params)
  prior_transform <- set_prior_transform(\(x) x, names = c("test 1", "test 2"))
  num_dim <- 2
  name <- "Test Sampler"
  description <- "A test sampler"

  sampler <- new_sampler(log_lik, prior_transform, num_dim, name, description)

  expect_s3_class(sampler, "ernest_sampler")
  expect_equal(sampler$log_lik, log_lik)
  expect_equal(sampler$prior_transform, prior_transform)
  expect_equal(sampler$num_dim, num_dim)
  expect_equal(sampler$name, name)
  expect_equal(sampler$description, description)
})

test_that("new_sampler checks input types", {
  expect_error(new_sampler(log_lik = "not a function"), "must be a function")
  expect_error(new_sampler(prior_transform = "not a prior_transform"), "must be an object of class 'prior_transform'")
  expect_error(new_sampler(num_dim = -1), "must be a whole number")
  expect_error(new_sampler(name = 123), "`name` must be a single string, not the number 123.")
  expect_error(new_sampler(description = 123), "`description` must be a single string, not the number 123.")
})

test_that("refresh_sampler refreshes the sampler", {
  log_lik <- function(params) sum(params)
  prior_transform <- set_prior_transform(\(x) x, names = c("test 1", "test 2"))
  num_dim <- 2
  name <- "Test Sampler"
  description <- "A test sampler"

  sampler <- new_sampler(log_lik, prior_transform, num_dim, name, description)
  refreshed_sampler <- refresh_sampler(sampler)

  expect_equal(refreshed_sampler, sampler)
})

test_that("update_sampler updates existing elements", {
  log_lik <- function(params) sum(params)
  prior_transform <- set_prior_transform(\(x) x, names = c("test 1", "test 2"))
  num_dim <- 2
  name <- "Test Sampler"
  description <- "A test sampler"

  sampler <- new_sampler(log_lik, prior_transform, num_dim, name, description)
  updated_sampler <- update_sampler(sampler, name = "Updated Sampler")

  expect_equal(updated_sampler$name, "Updated Sampler")
  expect_equal(updated_sampler$log_lik, log_lik)
  expect_equal(updated_sampler$prior_transform, prior_transform)
  expect_equal(updated_sampler$num_dim, num_dim)
  expect_equal(updated_sampler$description, description)
})

test_that("update_sampler checks for existing elements", {
  log_lik <- function(params) sum(params)
  prior_transform <- set_prior_transform(\(x) x, names = c("test 1", "test 2"))
  num_dim <- 2
  name <- "Test Sampler"
  description <- "A test sampler"

  sampler <- new_sampler(log_lik, prior_transform, num_dim, name, description)

  expect_error(update_sampler(sampler, new_element = "value"), "must already exist")
})

test_that("propose_uniform proposes a valid new particle", {
  log_lik <- function(params) sum(params)
  prior_transform <- set_prior_transform(\(x) x, names = c("test 1", "test 2"))
  sampler <- new_sampler(log_lik, prior_transform, num_dim = 2, name = "Test Sampler", description = "Test Description")

  particle <- propose_uniform(sampler, min_lik = -1)
  expect_named(particle, c("unit", "parameter", "log_lik", "num_calls"))
  expect_length(particle$unit, 2)
  expect_vector(particle$unit, ptype = double(), size = 2)
  expect_vector(particle$parameter, ptype = double(), size = 2)
  expect_true(particle$log_lik > -1)
  expect_type(particle$num_calls, "integer")
})

test_that("propose_live defaults to propose_uniform for base class", {
  log_lik <- function(params) sum(params)
  prior_transform <- set_prior_transform(\(x) x, names = c("test 1", "test 2"))
  sampler <- new_sampler(log_lik, prior_transform, num_dim = 2, name = "Test Sampler", description = "Test Description")

  original <- list(unit = c(0.5, 0.5), parameter = c(0.5, 0.5), log_lik = 0, num_calls = 1)
  expect_warning(
    particle <- propose_live(sampler, original, min_lik = 0)
  )
  expect_named(particle, c("unit", "parameter", "log_lik", "num_calls"))
  expect_length(particle$unit, 2)
  expect_vector(particle$unit, ptype = double(), size = 2)
  expect_vector(particle$parameter, ptype = double(), size = 2)
  expect_true(particle$log_lik > 0)
  expect_type(particle$num_calls, "integer")
})
