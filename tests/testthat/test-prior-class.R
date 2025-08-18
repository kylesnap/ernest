test_that("create_prior creates a custom prior", {
  # 3D uniform prior in [-10, 10]
  unif <- function(x) {
    -10 + x * 20
  }

  # Single vector input
  prior <- create_prior(unif, n_dim = 3, lower = -10, upper = 10)
  expect_equal(prior$fn(c(0.25, 0.5, 0.75)), c(-5, 0, 5))
  mat <- matrix(c(0.25, 0.5, 0.75, 0.1, 0.2, 0.3), ncol = 3, byrow = TRUE)
  expect_equal(
    prior$fn(mat),
    matrix(c(-5, 0, 5, -8, -6, -4), ncol = 3, byrow = TRUE)
  )
})

test_that("create_prior creates a custom rowwise prior", {
  # 3D uniform prior in [-10, 10]
  unif <- function(x) {
    -10 + x * 20
  }

  # Single vector input
  prior <- create_prior(rowwise = unif, n_dim = 3, lower = -10, upper = 10)
  expect_equal(prior$fn(c(0.25, 0.5, 0.75)), c(-5, 0, 5))
  mat <- matrix(c(0.25, 0.5, 0.75, 0.1, 0.2, 0.3), ncol = 3, byrow = TRUE)
  expect_equal(
    prior$fn(mat),
    matrix(c(-5, 0, 5, -8, -6, -4), ncol = 3, byrow = TRUE)
  )
  expect_snapshot(prior)
})

test_that("check_prior_params throws errors for improper inputs", {
  expect_snapshot_error(check_prior_params(n_dim = -1L, varnames = "X"))
  expect_snapshot_error(check_prior_params(n_dim = c(1L, 2L), varnames = "X"))
  expect_snapshot_error(check_prior_params(n_dim = 2, varnames = 1))
  expect_snapshot_error(check_prior_params(
    n_dim = 2,
    varnames = c("a", "b", "c")
  ))
  expect_snapshot_error(
    check_prior_params(
      n_dim = 2,
      varnames = "X",
      lower = c(0, 0),
      upper = c(0, 0)
    )
  )
})

test_that("check_prior_params works as expected", {
  default_obj <- check_prior_params(
    n_dim = 3,
    varnames = "X",
    lower = -Inf,
    upper = Inf
  )
  expect_mapequal(
    default_obj,
    list(
      n_dim = 3L,
      varnames = c("X", "X.1", "X.2"),
      lower = rep(-Inf, 3),
      upper = rep(Inf, 3)
    )
  )

  fancy_obj <- check_prior_params(
    n_dim = 5,
    varnames = c("int", "slope1", "slope2", "var1", "var2"),
    lower = c(-Inf, -Inf, -Inf, 0, 0),
    upper = Inf
  )
  expect_mapequal(
    fancy_obj,
    list(
      n_dim = 5L,
      varnames = c("int", "slope1", "slope2", "var1", "var2"),
      lower = c(-Inf, -Inf, -Inf, 0, 0),
      upper = rep(Inf, 5)
    )
  )
})

test_that("create_prior errors if prior function output length is wrong", {
  fn <- function(x) c(x[1], x[2], x[1] / x[2])
  expect_snapshot_error(check_prior_fn(fn, n_dim = 2))

  fn <- function(x) {
    if (is.matrix(x)) {
      matrix(runif(20), ncol = 5)
    } else {
      runif(2)
    }
  }
  expect_snapshot_error(create_prior(rowwise = fn, n_dim = 2))
  expect_no_message(create_prior(fn, n_dim = 2))
})

test_that("create_prior errors if prior returns non-finite values", {
  fn <- function(x) rep(NaN, length(x))
  expect_snapshot_error(create_prior(fn, n_dim = 2))
})

test_that("create_prior errors with invalid bounds", {
  fn <- function(x) x
  upper <- c(-Inf, -Inf)
  expect_snapshot_error(
    create_prior(fn, n_dim = 2, lower = c(-Inf, -Inf), upper = upper)
  )

  lower <- c(-Inf, 1)
  expect_snapshot_error(
    create_prior(fn, n_dim = 2, lower = lower, upper = c(Inf, Inf)),
  )
})
