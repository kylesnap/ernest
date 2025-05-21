test_that("ernest_prior creates a valid object", {
  dist <- distributional::dist_normal(mu = 0, sigma = 1)
  prior <- ernest_prior(dist, names = "x")

  expect_s3_class(prior, "ernest_prior")
  expect_equal(variables(prior), "x")
  expect_equal(nvariables(prior), 1)
})

test_that("ernest_prior validates input types", {
  expect_error(ernest_prior(1:10), "no applicable method")
  expect_error(
    ernest_prior(distributional::dist_normal(), names = 1),
    "must be a character vector"
  )
})


test_that("ernest_prior handles empty names", {
  dist <- distributional::dist_normal(mu = 0, sigma = 1)
  expect_message(prior <- ernest_prior(dist), "New names")

  expect_equal(variables(prior), c("...1"))
  expect_equal(nvariables(prior), 1)
})

test_that("validate_ernest_prior checks unsupported distributions", {
  dist <- distributional::dist_missing()
  expect_error(
    ernest_prior(c("x" = dist), repair = "unique_quiet"),
    "Some distributions are NULL"
  )

  dist <- c(
    distributional::dist_normal(0, 1),
    distributional::dist_multinomial(size = 4, prob = c(0.3, 0.5, 0.2))
  )
  expect_error(
    ernest_prior(dist, repair = "unique_quiet"),
    "The following distributions are not currently supported: Multinomial"
  )
})

test_that("validate_ernest_prior handles truncated distributions", {
  dist <- distributional::dist_truncated(
    distributional::dist_pareto(10, 1),
    lower = 1
  )
  expect_error(
    ernest_prior(c("x" = dist)),
    "The following truncated distributions are not currently supported: Pareto"
  )
})

test_that("renaming fails on duplicates", {
  dist <- c(
    distributional::dist_normal(0, 1),
    distributional::dist_normal(0, 1)
  )
  expect_message(
    prior <- ernest_prior(dist, names = c("x", "x"))
  )
  expect_named(prior, c("x...1", "x...2"))

  expect_error(
    variables(prior) <- c("y", "y"),
    "The following names are duplicated: y."
  )
})

test_that("ernest_prior printing", {
  dist <- c(
    "int" = distributional::dist_normal(0, 10),
    "coef" = distributional::dist_normal(0, 10),
    "sigma" = distributional::dist_truncated(
      distributional::dist_cauchy(0, 1),
      0
    )
  )

  prior <- ernest_prior(dist)
  expect_snapshot(print(prior))
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

  prior <- ernest_prior(distrs, repair = "unique_quiet")
  fn <- compile(prior)
  expect_equal(apply(test, 1, fn), t(key))
})

test_that("truncated distributions are handled correctly", {
  dist <- c(
    distributional::dist_normal(0, 1),
    distributional::dist_truncated(distributional::dist_normal(0, 1), -5, 5)
  )
  prior <- ernest_prior(dist, repair = "unique_quiet")
  fn <- compile(prior)
  expect_equal(
    fn(c(0.1, 0.1)),
    c(-1.2815515655, -1.2815502588)
  )
  expect_equal(
    fn(c(0.5, 0.5)),
    c(0, -1.391458e-16)
  )
})

test_that("Custom distributions are handled correctly", {
  prior <- ernest_prior(\(x) qnorm(x), names = LETTERS[1:10])
  fn <- compile(prior)
  expect_equal(
    fn(c(0.1, 0.2, 0.3)),
    qnorm(c(0.1, 0.2, 0.3))
  )
  expect_equal(
    variables(prior),
    LETTERS[1:10]
  )
  variables(prior) <- LETTERS[11:20]
  expect_equal(
    variables(prior),
    LETTERS[11:20]
  )
  expect_equal(nvariables(prior), 10)
})
