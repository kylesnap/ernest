set.seed(42)
test_mat <- matrix(runif(100 * 3), nrow = 100)

test_that("create_normal_prior tests", {
  expect_error(
    create_normal_prior(create_normal_prior(3, sd = c(0, 1, 1))),
    "`sd` of a normal distribution must be non-negative."
  )

  prior <- create_normal_prior(3L)
  expect_s3_class(prior, "ernest_prior")
  expect_equal(prior$varnames, c("N(0, 1)", "N(0, 1).1", "N(0, 1).2"))
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

test_that("create_t_prior returns correct object and values", {
  expect_error(
    create_t_prior(3, df = c(0, 1, 1)),
    "`df` of Student's t distribution must be non-negative."
  )
  expect_error(
    create_t_prior(3, df = c(1, 2, 3), sigma = c(0, 1, 1)),
    "`sigma` of Student's t distribution must be non-negative."
  )

  prior <- create_t_prior(3L, df = c(1, 2, 5))
  expect_s3_class(prior, "ernest_prior")
  expect_equal(prior$varnames, c("T(1, 0, 1)", "T(2, 0, 1)", "T(5, 0, 1)"))
  expect_equal(prior$n_dim, 3L)

  expect_equal(
    prior$fn(c(0.5, 0.5, 0.5)),
    c(qt(0.5, 1), qt(0.5, 2), qt(0.5, 5))
  )

  test_mat <- matrix(runif(100 * 3), nrow = 100)
  expect_equal(
    prior$fn(test_mat),
    t(apply(test_mat, 1, function(x) c(qt(x[1], 1), qt(x[2], 2), qt(x[3], 5))))
  )

  skip_if_not_installed("LaplacesDemon")
  prior <- create_t_prior(
    3,
    df = c(1, 2, 5),
    mu = c(0, 1, 2),
    sigma = c(1, 2, 3),
    lower = c(0, -Inf, -Inf),
    upper = c(Inf, Inf, 1)
  )
  expected_transformation <- function(x) {
    c(
      LaplacesDemon::qtrunc(x[1], "st", a = 0, nu = 1, mu = 0, sigma = 1),
      LaplacesDemon::qtrunc(x[2], "st", nu = 2, mu = 1, sigma = 2),
      LaplacesDemon::qtrunc(x[3], "st", b = 1, nu = 5, mu = 2, sigma = 3)
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

test_that("create_cauchy_prior returns correct object and values", {
  expect_error(
    create_cauchy_prior(3, scale = c(0, 1, 1)),
    "`scale` of the Cauchy distribution must be non-negative."
  )

  prior <- create_cauchy_prior(3L)
  expect_s3_class(prior, "ernest_prior")
  expect_equal(
    prior$varnames,
    c("Cauchy(0, 1)", "Cauchy(0, 1).1", "Cauchy(0, 1).2")
  )
  expect_equal(prior$n_dim, 3L)

  expect_equal(
    prior$fn(c(0.5, 0.5, 0.5)),
    c(qcauchy(0.5, 0, 1), qcauchy(0.5, 0, 1), qcauchy(0.5, 0, 1))
  )

  test_mat <- matrix(runif(100 * 3), nrow = 100)
  expect_equal(
    prior$fn(test_mat),
    t(apply(test_mat, 1, function(x) {
      c(qcauchy(x[1], 0, 1), qcauchy(x[2], 0, 1), qcauchy(x[3], 0, 1))
    }))
  )

  skip_if_not_installed("LaplacesDemon")
  prior <- create_cauchy_prior(
    4,
    location = c(0, 0, 0, -2),
    scale = c(0.5, 1, 2, 1),
    lower = c(0, 0, -Inf, -Inf),
    upper = c(Inf, 1, Inf, 1)
  )
  expected_transformation <- function(x) {
    c(
      LaplacesDemon::qtrunc(x[1], "cauchy", a = 0, location = 0, scale = 0.5),
      LaplacesDemon::qtrunc(
        x[2],
        "cauchy",
        a = 0,
        b = 1,
        location = 0,
        scale = 1
      ),
      LaplacesDemon::qtrunc(x[3], "cauchy", location = 0, scale = 2),
      LaplacesDemon::qtrunc(x[4], "cauchy", b = 1, location = -2, scale = 1)
    )
  }
  test_mat <- matrix(runif(1000 * 4), nrow = 1000)
  expect_equal(
    prior$fn(c(0.5, 0.5, 0.5, 0.5)),
    expected_transformation(c(0.5, 0.5, 0.5, 0.5))
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
    prior$varnames,
    c("Uniform(0, 1)", "Uniform(-1, 0)", "Uniform(-2, 1)")
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
  expect_snapshot(create_t_prior(1, df = 1))
  expect_snapshot(create_cauchy_prior(1, location = 0, scale = 1))
  expect_snapshot(create_uniform_prior(1, lower = 0, upper = 1))
})
