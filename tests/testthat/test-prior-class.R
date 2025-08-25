#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("new_ernest_prior throws informative errors", {
  fn <- function(x) x
  expect_snapshot_error(new_ernest_prior(fn, n_dim = 0))
  expect_snapshot_error(new_ernest_prior(
    fn,
    n_dim = 3,
    lower = c(-Inf, 0)
  ))
  expect_snapshot_error(new_ernest_prior(
    fn,
    n_dim = 3,
    lower = c("-Inf", 0, 0)
  ))
  expect_snapshot_error(new_ernest_prior(
    fn,
    n_dim = 3,
    lower = c(0, 0),
    upper = c(1, 1),
    varnames = c("A", "B", "C")
  ))
})

test_that("new_ernest_prior errors if lower >= upper", {
  fn <- function(x) x
  expect_snapshot_error(
    new_ernest_prior(fn, n_dim = 2, lower = c(1, 2), upper = c(2, 2))
  )
  expect_snapshot_error(
    new_ernest_prior(fn, n_dim = 2, lower = c(2, 2), upper = c(1, 1))
  )
})

test_that("new_ernest_prior returns correct structure and class", {
  fn <- function(x) x
  prior <- new_ernest_prior(
    prior_fn = fn,
    n_dim = 2,
    lower = c(0, 1),
    upper = c(1, 2),
    varnames = c("a", "b")
  )
  expect_type(prior, "list")
  expect_s3_class(prior, "ernest_prior")
  expect_equal(prior$n_dim, 2)
  expect_equal(prior$lower, c(0, 1))
  expect_equal(prior$upper, c(1, 2))
  expect_equal(attr(prior, "varnames"), c("a", "b"))
  expect_true(is.function(prior$fn))
})

test_that("new_ernest_prior recycles varnames, lower, upper", {
  fn <- function(x) x
  prior <- new_ernest_prior(
    prior_fn = fn,
    n_dim = 3,
    lower = 0,
    upper = 1,
    varnames = "x"
  )
  expect_equal(prior$lower, rep(0, 3))
  expect_equal(prior$upper, rep(1, 3))
  expect_equal(attr(prior, "varnames"), c("x...1", "x...2", "x...3"))
})

test_that("create_prior can apply function arguments", {
  fn <- function(x) x
  prior <- create_prior(
    fn = stats::qnorm,
    mean = !!c(-1, 0, 1),
    sd = !!c(1, 1, 1),
    .n_dim = 3,
    .varnames = "x"
  )
  expect_equal(attr(prior, "varnames"), c("x...1", "x...2", "x...3"))
  expect_equal(
    prior$fn(c(0.5, 0.5, 0.5)),
    c(-1, 0, 1)
  )
})

test_that("new_ernest_prior repairs names as specified", {
  fn <- function(x) x
  prior <- new_ernest_prior(
    prior_fn = fn,
    n_dim = 2,
    varnames = c("x", "x"),
    repair = "universal"
  )
  expect_equal(attr(prior, "varnames"), c("x...1", "x...2"))
  expect_snapshot_error(
    new_ernest_prior(
      prior_fn = fn,
      n_dim = 2,
      varnames = c("x", "x"),
      repair = "check_unique"
    )
  )
})

test_that("create_prior creates a custom prior", {
  # 3D uniform prior in [-10, 10]
  unif <- function(x) {
    -10 + x * 20
  }

  # Single vector input
  prior <- create_prior(unif, .n_dim = 3, .lower = -10, .upper = 10)
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
  prior <- create_prior(rowwise = unif, .n_dim = 3, .lower = -10, .upper = 10)
  expect_equal(prior$fn(c(0.25, 0.5, 0.75)), c(-5, 0, 5))
  mat <- matrix(c(0.25, 0.5, 0.75, 0.1, 0.2, 0.3), ncol = 3, byrow = TRUE)
  expect_equal(
    prior$fn(mat),
    matrix(c(-5, 0, 5, -8, -6, -4), ncol = 3, byrow = TRUE)
  )
  expect_snapshot(prior)
})

test_that("create_prior errors if prior function output length is wrong", {
  fn <- function(x) c(x[1], x[2], x[1] / x[2])
  expect_snapshot_error(
    create_prior(fn, .n_dim = 2)
  )

  fn <- function(x) {
    if (is.matrix(x)) {
      matrix(runif(20), ncol = 5)
    } else {
      runif(2)
    }
  }
  expect_snapshot_error(create_prior(rowwise = fn, .n_dim = 2))
  expect_no_message(create_prior(fn, .n_dim = 2))
})

test_that("create_prior errors if prior returns non-finite values", {
  fn <- function(x) rep(NaN, length(x))
  expect_snapshot_error(create_prior(fn, .n_dim = 2))
})
