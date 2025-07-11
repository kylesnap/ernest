set.seed(42)

test_that("create_prior returns a valid ernest_prior object with correct attributes", {
  fn <- function(x) rep(1, length(x))
  varnames <- c("a", "b", "c")
  prior <- create_prior(fn, n_dim = 3, varnames = varnames)
  expect_s3_class(prior, "ernest_prior")
  expect_equal(prior$varnames, varnames)
  expect_equal(prior$n_dim, 3)
})

test_that("create_prior handles lower and upper bounds recycling", {
  fn <- function(x) x
  varnames <- c("x", "y")
  lower <- c(0)
  upper <- c(1)
  prior <- create_prior(
    fn,
    n_dim = 2,
    varnames = varnames,
    lower = lower,
    upper = upper
  )
  expect_equal(prior$lower, c(0, 0))
  expect_equal(prior$upper, c(1, 1))
})

test_that("create_prior silently repairs var. names", {
  fn <- function(x) x
  varnames <- c("a", "a")
  prior <- create_prior(fn, n_dim = 2, varnames = varnames)
  expect_equal(prior$varnames, c("a", "a.1"))
})

test_that("create_prior errors if n_dim < 1", {
  fn <- function(x) numeric(0)
  varnames <- character(0)
  expect_snapshot_error(create_prior(fn, n_dim = 0, varnames = varnames))
})

test_that("create_prior errors if prior function output length is wrong", {
  fn <- function(x) c(x[1], x[2], x[1] / x[2])
  varnames <- c("a", "b")
  expect_snapshot_error(create_prior(fn, n_dim = 2, varnames = varnames))
})

test_that("create_prior errors if prior returns non-finite values", {
  fn <- function(x) rep(NaN, length(x))
  varnames <- c("a", "b")
  expect_snapshot_error(create_prior(fn, n_dim = 2, varnames = varnames))
})

test_that("create_prior errors if lower > min(prior)", {
  fn <- function(x) rep(0, length(x))
  varnames <- c("a", "b")
  lower <- c(1, 1)
  expect_snapshot_error(create_prior(
    fn,
    n_dim = 2,
    varnames = varnames,
    lower = lower
  ))
})

test_that("create_prior errors if lower >= upper", {
  fn <- function(x) ifelse(x == 0, 1, -1)
  varnames <- c("a", "b")
  lower <- c(1, 1)
  upper <- c(-1, -1)
  expect_snapshot_error(create_prior(
    fn,
    n_dim = 2,
    varnames = varnames,
    lower = lower,
    upper = upper
  ))
})

test_that("create_normal_prior returns correct object and values", {
  skip_if_not_installed("LaplacesDemon")
  prior <- create_normal_prior(1L, mean = 0, sd = 1)
  expect_s3_class(prior, "ernest_prior")
  expect_equal(prior$varnames, "N(0, 1)")
  expect_equal(prior$n_dim, 1)
  test_vec <- seq(0, 1, by = 0.01)
  expect_equal(
    vapply(test_vec, prior$fn, numeric(1)),
    vapply(test_vec, qnorm, numeric(1))
  )

  skip_if_not_installed("LaplacesDemon")
  prior <- create_normal_prior(
    mean = c(1, 0, 0),
    sd = c(1, 1, 10),
    n_dim = 3,
    lower = 0,
    upper = c(Inf, Inf, 1)
  )
  expect_equal(
    prior$varnames,
    c("N(1, 1)", "N(0, 1)", "N(0, 100)")
  )
  test_mat <- matrix(runif(1000 * 3), nrow = 1000)
  expect_equal(
    apply(test_mat, 1, prior$fn),
    apply(test_mat, 1, \(x) {
      c(
        LaplacesDemon::qtrunc(x[1], "norm", a = 0, mean = 1, sd = 1),
        LaplacesDemon::qtrunc(x[2], "norm", a = 0, mean = 0, sd = 1),
        LaplacesDemon::qtrunc(x[3], "norm", a = 0, b = 1, mean = 0, sd = 10)
      )
    })
  )
})

test_that("create_t_prior returns correct object and values", {
  prior <- create_t_prior(1, df = 1)
  expect_s3_class(prior, "ernest_prior")
  expect_equal(prior$varnames, "T(1, 0, 1)")
  expect_equal(prior$n_dim, 1)
  test_vec <- seq(0, 1, by = 0.01)
  expect_equal(
    vapply(test_vec, prior$fn, numeric(1)),
    vapply(test_vec, function(x) qt(x, 1), numeric(1))
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
  expect_equal(
    prior$varnames,
    c("T(1, 0, 1)", "T(2, 1, 2)", "T(5, 2, 3)")
  )
  test_mat <- matrix(runif(1000 * 3), nrow = 1000)
  expect_equal(
    t(apply(test_mat, 1, prior$fn)),
    t(apply(test_mat, 1, \(x) {
      c(
        LaplacesDemon::qtrunc(x[1], "st", a = 0, nu = 1, mu = 0, sigma = 1),
        LaplacesDemon::qtrunc(x[2], "st", nu = 2, mu = 1, sigma = 2),
        LaplacesDemon::qtrunc(x[3], "st", b = 1, nu = 5, mu = 2, sigma = 3)
      )
    }))
  )
})

test_that("create_cauchy_prior returns correct object and values", {
  prior <- create_cauchy_prior(1, location = 0, scale = 1)
  expect_s3_class(prior, "ernest_prior")
  expect_equal(prior$varnames, "Cauchy(0, 1)")
  expect_equal(prior$n_dim, 1)
  test_vec <- seq(0, 1, by = 0.01)
  expect_equal(
    vapply(test_vec, prior$fn, numeric(1)),
    vapply(test_vec, qcauchy, numeric(1))
  )

  skip_if_not_installed("LaplacesDemon")
  prior <- create_cauchy_prior(
    4,
    location = c(0, 0, 0, -2),
    scale = c(0.5, 1, 2, 1),
    lower = c(0, 0, -Inf, -Inf),
    upper = c(Inf, 1, Inf, 1)
  )
  expect_equal(
    prior$varnames,
    c("Cauchy(0, 0.5)", "Cauchy(0, 1)", "Cauchy(0, 2)", "Cauchy(-2, 1)")
  )
  test_mat <- matrix(runif(1000 * 4), nrow = 1000)
  expect_equal(
    t(apply(test_mat, 1, prior$fn)),
    t(apply(test_mat, 1, \(x) {
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
    }))
  )
})

test_that("create_uniform_prior returns correct object and values", {
  prior <- create_uniform_prior(1, lower = 0, upper = 1)
  expect_s3_class(prior, "ernest_prior")
  expect_equal(prior$varnames, "Uniform(0, 1)")
  expect_equal(prior$n_dim, 1)
  test_vec <- seq(0, 1, by = 0.01)
  expect_equal(
    vapply(test_vec, prior$fn, numeric(1)),
    vapply(test_vec, qunif, numeric(1))
  )

  prior <- create_uniform_prior(3, lower = c(0, -1, -2), upper = c(1, 0, 1))
  expect_equal(
    prior$varnames,
    c("Uniform(0, 1)", "Uniform(-1, 0)", "Uniform(-2, 1)")
  )
  test_mat <- matrix(runif(1000 * 3), nrow = 1000)
  expect_equal(
    t(apply(test_mat, 1, prior$fn)),
    t(apply(test_mat, 1, \(x) {
      c(
        qunif(x[1], 0, 1),
        qunif(x[2], -1, 0),
        qunif(x[3], -2, 1)
      )
    }))
  )
})

test_that("Print methods", {
  expect_snapshot(create_normal_prior(1, mean = 0, sd = 1))
  expect_snapshot(create_t_prior(1, df = 1))
  expect_snapshot(create_cauchy_prior(1, location = 0, scale = 1))
  expect_snapshot(create_uniform_prior(1, lower = 0, upper = 1))
  expect_snapshot(create_prior(function(x) x, n_dim = 1, varnames = "x"))
})
