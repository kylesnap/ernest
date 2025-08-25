set.seed(42)
test_mat <- matrix(runif(100 * 3), nrow = 100)

test_that("create_normal_prior error handling", {
  expect_snapshot_error(create_normal_prior(3, sd = c(0, 1, 1)))

  prior <- create_normal_prior(3L)
  expect_s3_class(prior, "ernest_prior")
  expect_equal(
    attr(prior, "varnames"),
    c("Normal...1", "Normal...2", "Normal...3")
  )
  expect_equal(prior$n_dim, 3L)

  prior2 <- create_normal_prior(
    varnames = c("Normal...1", "Normal...2", "Normal...3")
  )
  expect_identical(prior, prior2)

  prior3 <- create_normal_prior(mean = c(0, 0, 0))
  expect_identical(prior, prior3)
})

test_that("create_normal_prior untruncated transformation", {
  prior <- create_normal_prior(
    mean = c(1, 2, 3),
    sd = c(1, 2, 3),
    n_dim = 3
  )

  expect_equal(
    prior$fn(c(0.5, 0.5, 0.5)),
    c(qnorm(0.5, 1, 1), qnorm(0.5, 2, 2), qnorm(0.5, 3, 3))
  )

  mat <- matrix(runif(100 * 3), nrow = 100)
  expect_equal(
    prior$fn(mat),
    t(apply(
      mat,
      1,
      function(x) c(qnorm(x[1], 1, 1), qnorm(x[2], 2, 2), qnorm(x[3], 3, 3))
    ))
  )
})

test_that("create_normal_prior truncated transformation", {
  skip_if_not_installed("truncnorm")
  prior <- create_normal_prior(
    mean = c(1, 2, 3),
    sd = 3,
    n_dim = 3,
    lower = c(0, -Inf, -Inf),
    upper = c(Inf, Inf, 1)
  )
  expected <- function(x) {
    c(
      truncnorm::qtruncnorm(x[1], a = 0, mean = 1, sd = 3),
      truncnorm::qtruncnorm(x[2], mean = 2, sd = 3),
      truncnorm::qtruncnorm(x[3], b = 1, mean = 3, sd = 3)
    )
  }
  expect_equal(prior$fn(c(0.5, 0.5, 0.5)), expected(c(0.5, 0.5, 0.5)))
  mat <- matrix(runif(50 * 3), nrow = 50)
  expect_equal(
    prior$fn(mat),
    t(apply(mat, 1, expected))
  )
})

test_that("create_uniform_prior transformation and properties", {
  prior <- create_uniform_prior(3, lower = c(0, -1, -2), upper = c(1, 0, 1))
  expect_s3_class(prior, "ernest_prior")
  expect_equal(
    attr(prior, "varnames"),
    c("Uniform...1", "Uniform...2", "Uniform...3")
  )
  expect_equal(
    prior$fn(c(0.5, 0.5, 0.5)),
    c(qunif(0.5, 0, 1), qunif(0.5, -1, 0), qunif(0.5, -2, 1))
  )
  mat <- matrix(runif(100 * 3), nrow = 100)
  expect_equal(
    prior$fn(mat),
    t(apply(
      mat,
      1,
      function(x) c(qunif(x[1], 0, 1), qunif(x[2], -1, 0), qunif(x[3], -2, 1))
    ))
  )
})

test_that("create_normal_prior and create_uniform_prior: print methods", {
  expect_snapshot(create_normal_prior(1, mean = 0, sd = 1))
  expect_snapshot(create_uniform_prior(1, lower = 0, upper = 1))
})
