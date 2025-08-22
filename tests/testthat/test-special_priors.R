set.seed(42)
test_mat <- matrix(runif(100 * 3), nrow = 100)

test_that("create_normal_prior tests", {
  expect_error(
    create_normal_prior(create_normal_prior(3, sd = c(0, 1, 1))),
    "`sd` of a normal distribution must be non-negative."
  )

  prior <- create_normal_prior(3L)
  expect_s3_class(prior, "ernest_prior")
  expect_equal(
    attr(prior, "varnames"),
    c("Normal...1", "Normal...2", "Normal...3")
  )
  expect_equal(prior$n_dim, 3L)

  expect_equal(prior$fn(c(0.5, 0.5, 0.5)), c(0, 0, 0))

  expect_equal(
    prior$fn(test_mat),
    t(apply(test_mat, 1, qnorm))
  )

  skip_if_not_installed("LaplacesDemon")
  prior <- create_normal_prior(
    mean = c(1, 2, 3),
    sd = 3,
    n_dim = 3,
    lower = c(0, -Inf, -Inf),
    upper = c(Inf, Inf, 1)
  )
  expected_transformation <- function(x) {
    c(
      LaplacesDemon::qtrunc(x[1], "norm", a = 0, mean = 1, sd = 3),
      LaplacesDemon::qtrunc(x[2], "norm", mean = 2, sd = 3),
      LaplacesDemon::qtrunc(x[3], "norm", b = 1, mean = 3, sd = 3)
    )
  }

  expect_equal(
    prior$fn(c(0.5, 0.5, 0.5)),
    expected_transformation(c(0.5, 0.5, 0.5))
  )
  expect_equal(
    prior$fn(test_mat),
    t(apply(test_mat, 1, expected_transformation))
  )
})

test_that("create_uniform_prior returns correct object and values", {
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

  test_mat <- matrix(runif(1000 * 3), nrow = 1000)
  expect_equal(
    prior$fn(test_mat),
    t(apply(test_mat, 1, function(x) {
      c(qunif(x[1], 0, 1), qunif(x[2], -1, 0), qunif(x[3], -2, 1))
    }))
  )
})

test_that("Print methods", {
  expect_snapshot(create_normal_prior(1, mean = 0, sd = 1))
  expect_snapshot(create_uniform_prior(1, lower = 0, upper = 1))
})
