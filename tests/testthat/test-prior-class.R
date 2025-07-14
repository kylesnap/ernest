test_that("check_prior_params throws errors for improper inputs", {
  expect_error(
    check_prior_params(n_dim = -1L, varnames = "X"),
    "whole number larger than or equal to 1"
  )
  expect_error(
    check_prior_params(n_dim = c(1L, 2L), varnames = "X"),
    "not an integer vector"
  )
  expect_error(
    check_prior_params(n_dim = 2, varnames = 1),
    "Can't convert `varnames` <double> to <character>."
  )
  expect_error(
    check_prior_params(n_dim = 2, varnames = c("a", "b", "c")),
    "Can't recycle"
  )
  expect_error(
    check_prior_params(
      n_dim = 2,
      varnames = "X",
      lower = c(0, 0),
      upper = c(0, 0)
    ),
    "`lower` must be strictly smaller than `upper`"
  )
})

test_that("check_prior_params works as expected", {
  default_obj <- check_prior_params(
    n_dim = 3,
    varnames = "X"
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
    lower = c(-Inf, -Inf, -Inf, 0, 0)
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
  expect_error(
    check_prior_fn(fn, n_dim = 2),
    "`fn` must return a vector of length 2."
  )

  fn <- function(x) {
    if (is.matrix(x)) {
      matrix(runif(20), ncol = 5)
    } else {
      runif(2)
    }
  }
  expect_error(
    check_prior_fn(fn, n_dim = 2),
    "`fn` must preserve the dimensions"
  )
})

test_that("create_prior errors if prior returns non-finite values", {
  fn <- function(x) rep(NaN, length(x))
  expect_error(
    check_prior_fn(fn, n_dim = 2),
    "`fn` must always return finite values."
  )
})

test_that("create_prior errors if lower > min(prior)", {
  fn <- function(x) x
  lower <- c(-Inf, 1)
  expect_error(
    check_prior_fn(fn, n_dim = 2, lower = lower, upper = c(Inf, Inf)),
    "`fn` must return values greater than or equal to `lower`."
  )
})

test_that("create_prior errors if lower >= upper", {
  fn <- function(x) x
  upper <- c(0, Inf)
  expect_error(
    check_prior_fn(fn, n_dim = 2, lower = c(-Inf, -Inf), upper = upper),
    "`fn` must return values lesser than or equal to `upper`."
  )
})

test_that("create_prior works with auto_batch = TRUE", {
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

test_that("create_prior works with a pre-batched function", {
  # 3D uniform prior in [-10, 10]
  unif <- function(x) {
    out <- stats::qunif(c(x), -10, 10)
    if (is.matrix(x)) {
      dim(out) <- dim(x)
    }
    out
  }

  prior <- create_prior(
    unif,
    n_dim = 3,
    lower = -10,
    upper = 10,
    auto_batch = FALSE
  )
  expect_equal(prior$fn(c(0.25, 0.5, 0.75)), c(-5, 0, 5))
  mat <- matrix(c(0.25, 0.5, 0.75, 0.1, 0.2, 0.3), ncol = 3, byrow = TRUE)
  expect_equal(
    prior$fn(mat),
    matrix(c(-5, 0, 5, -8, -6, -4), ncol = 3, byrow = TRUE)
  )
  expect_snapshot(prior)
})

test_that("Throw an informative error if the user sets auto_batch to FALSE", {
  # Beta-Bernoull Prior
  bb_p <- function(x) {
    beta <- qbeta(x[1], 5, 5)
    binom <- qbinom(x[2], size = 1, beta)
    c(beta, binom)
  }
  expect_error(
    create_prior(bb_p, n_dim = 2, auto_batch = FALSE),
    "Can `fn` handle matrices without setting `auto_batch = TRUE`?"
  )
})
