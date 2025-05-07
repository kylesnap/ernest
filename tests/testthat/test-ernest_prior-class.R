test_that("ernest_prior.function creates a valid prior object", {
  fn <- function(x) x * 2
  variables <- c("var1", "var2")
  prior <- ernest_prior(fn, variables)

  expect_s3_class(prior, "ernest_prior")
  expect_equal(prior$variables, variables)
  expect_null(prior$dist)
  expect_true(is.function(prior$fn))
})

test_that("ernest_prior.function handles numeric variables", {
  fn <- function(x) x * 2
  prior <- ernest_prior(fn, 3)

  expect_s3_class(prior, "ernest_prior")
  expect_equal(length(prior$variables), 3)
  expect_null(prior$dist)
  expect_true(is.function(prior$fn))
})

test_that("ernest_prior.distribution creates a valid prior object", {
  dist <- c(
    "int" = distributional::dist_normal(0, 10),
    "coef" = distributional::dist_normal(0, 10),
    "sigma" = distributional::dist_truncated(distributional::dist_cauchy(0, 1))
  )

  prior <- ernest_prior(dist)
  expect_true(is.function(prior$fn))
  expect_equal(prior$dist, unname(dist))
  expect_equal(prior$variables, c("int", "coef", "sigma"))
})

test_that("ernest_prior.dist_default throws an error for unsupported distributions", {
  dist <- c(distributional::dist_normal(0, 10),
            distributional::dist_normal(0, 10),
            distributional::dist_missing())
  expect_error(ernest_prior(dist), "Some distributions are NULL")

  dist <- c(distributional::dist_normal(0, 10),
            distributional::dist_normal(0, 10),
            distributional::dist_categorical(c(0.05, 0.5, 0.15)))
  expect_error(ernest_prior(dist), "does not support")
})

test_that("ernest_prior.dist_* correctly forms a transformation function", {
  distrs <- c(
    distributional::dist_beta(shape1 = 1, shape2 = 1),
    distributional::dist_binomial(size = 1, p = 0.5),
    distributional::dist_cauchy(location = 0, scale = 1),
    distributional::dist_chisq(df = 1),
    distributional::dist_exponential(rate = 1),
    distributional::dist_f(df1 = 1, df2 = 1),
    distributional::dist_gamma(shape = 1, rate = 1),
    distributional::dist_geometric(p = 0.5),
    distributional::dist_hypergeometric(m = 1, n = 1, k = 1),
    distributional::dist_lognormal(mu = 0, sigma = 1),
    distributional::dist_negative_binomial(size = 1, p = 0.5),
    distributional::dist_normal(mu = 0, sigma = 1),
    distributional::dist_poisson(lambda = 1),
    distributional::dist_student_t(df = 1),
    distributional::dist_uniform(min = 0, max = 1),
    distributional::dist_weibull(shape = 1, scale = 1)
  )
  test <- readRDS(test_path("prior_test_matrix.rds"))
  key <- readRDS(test_path("prior_key_matrix.rds"))

  prior <- ernest_prior(distrs)
  expect_equal(prior$fn(test), key)
  expect_equal(prior$fn(test[1,,drop=FALSE]), key[1,,drop=FALSE])
  expect_equal(prior$fn(test[c(6,9),,drop=FALSE]), key[c(6,9),,drop=FALSE])
})

test_that("truncated distributions are handled correctly", {
  dist <- c(
    distributional::dist_normal(0, 1),
    distributional::dist_truncated(distributional::dist_normal(0, 1), -5, 5)
  )
  prior <- ernest_prior(dist)

  expect_equal(
    prior$fn(matrix(c(0.1, 0.1), nrow = 1, ncol = 2)),
    matrix(c(-1.2815515655, -1.2815502588), nrow = 1, ncol = 2)
  )
  expect_equal(
    prior$fn(matrix(c(0.5, 0.5), nrow = 1, ncol = 2)),
    matrix(c(0, -1.391458e-16), nrow = 1, ncol = 2)
  )
})

test_that("new_ernest_prior constructs a valid object", {
  variables <- c("var1", "var2")
  fn <- function(x) x * 2
  dist <- structure(
    list(shape1 = 2, shape2 = 5),
    class = c("dist_beta", "distribution")
  )
  prior <- new_ernest_prior(variables, fn, dist)

  expect_s3_class(prior, "ernest_prior")
  expect_equal(prior$variables, variables)
  expect_equal(prior$dist, dist)
  expect_true(is.function(prior$fn))
})

test_that("new_ernest_prior validates inputs", {
  expect_error(new_ernest_prior(123, function(x) x), "must be a character vector")
  expect_error(new_ernest_prior(c("var1"), "not_a_function"), "must be a function")
  expect_error(new_ernest_prior(c("var1"), function(x) x, "not_a_distribution"), "a distribution vector")
})

test_that("ernest_prior printing", {
  dist <- c(
    "int" = distributional::dist_normal(0, 10),
    "coef" = distributional::dist_normal(0, 10),
    "sigma" = distributional::dist_truncated(distributional::dist_cauchy(0, 1), 0)
  )

  prior <- ernest_prior(dist)
  expect_snapshot(print(prior))

  fn <- function(x) x * 2
  prior <- ernest_prior(fn, 3)
  expect_snapshot(print(prior))
})

test_that("ernest_prior variable/nvariable", {
  dist <- c(
    "int" = distributional::dist_normal(0, 10),
    "coef" = distributional::dist_normal(0, 10),
    "sigma" = distributional::dist_truncated(distributional::dist_cauchy(0, 1), 0)
  )

  prior <- ernest_prior(dist)
  expect_equal(variables(prior), c("int", "coef", "sigma"))
  expect_equal(nvariables(prior), 3)

  fn <- function(x) x * 2
  prior <- ernest_prior(fn, 3)
  expect_equal(variables(prior), c("", "", ""))
  expect_equal(nvariables(prior), 3)
})
