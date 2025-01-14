test_that("prior_transform.default throws an error for invalid input", {
  expect_error(prior_transform(42), "The prior transformation must be either a distrubtion3 object or a valid R function.")
})

test_that("prior_transform.distribution creates a valid prior_transform object", {
  dist <- distributions3::Normal(0, 1)
  pt <- prior_transform(dist, name = "normal_prior")

  expect_s3_class(pt, "prior_transform")
  expect_equal(pt$name, "normal_prior")
  expect_equal(pt$description, "Normal(mu = 0, sigma = 1)")
  expect_equal(pt$support, c(-Inf, Inf))
  expect_true(is.function(pt$fn))
})

test_that("prior_transform.function creates a valid prior_transform object", {
  fn <- function(q) q * 2
  pt <- prior_transform(fn, name = "linear_prior", support = c(0, 2))

  expect_s3_class(pt, "prior_transform")
  expect_equal(pt$name, "linear_prior")
  expect_equal(pt$description, "User-provided function")
  expect_equal(pt$support, c(0, 2))
  expect_true(is.function(pt$fn))
})

test_that("prior_transform.function infers support if not provided", {
  fn <- function(q) q * 2
  pt <- prior_transform(fn, name = "linear_prior")

  expect_equal(pt$support, c(0, 2))
})

test_that("validate_prior_transform checks for valid prior_transform object", {
  valid_fn <- function(q) q * 2
  valid_pt <- new_prior_transform(valid_fn, "valid_prior", "Valid description", c(0, 2))

  expect_silent(validate_prior_transform(valid_pt))

  invalid_pt <- valid_pt
  invalid_pt$fn <- NULL
  expect_error(validate_prior_transform(invalid_pt), "The `transform_function` must be a valid R function.")

  invalid_pt <- valid_pt
  invalid_pt$name <- 123
  expect_error(validate_prior_transform(invalid_pt), "The `name` must be a character string or NULL.")

  invalid_pt <- valid_pt
  invalid_pt$description <- 123
  expect_error(validate_prior_transform(invalid_pt), "The `description` must be a character string or NULL.")

  invalid_pt <- valid_pt
  invalid_pt$support <- c(2, 1)
  expect_error(validate_prior_transform(invalid_pt), "The lower bound of the support must be less than the upper bound.")
})
