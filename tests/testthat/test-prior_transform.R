test_that("prior_transform.distribution works correctly", {
  dist1 <- distributional::dist_normal(0, 5)
  dist2 <- distributional::dist_normal(5, 2)

  pt <- prior_transform(dist1 = dist1, dist2 = dist2)

  expect_s3_class(pt, "prior_transform")
  expect_equal(pt$dim, 2)
  expect_equal(pt$names, c("dist1", "dist2"))
  expect_equal(length(pt$lb), 2)
  expect_equal(length(pt$ub), 2)
})

test_that("prior_transform.function works correctly", {
  fn <- function(params) {
    c(params[1] * 2, params[2] + 3)
  }

  pt <- prior_transform(fn = fn, num_dim = 2, .names = c("param1", "param2"))

  expect_s3_class(pt, "prior_transform")
  expect_equal(pt$dim, 2)
  expect_equal(pt$names, c("param1", "param2"))
  expect_equal(length(pt$lb), 2)
  expect_equal(length(pt$ub), 2)
})

test_that("new_prior_transform constructor works correctly", {
  pt <- new_prior_transform(
    fn = \(x) sum(x),
    dim = 2,
    names = c("param1", "param2"),
    lb = c(0, 0),
    ub = c(1, 1)
  )

  expect_s3_class(pt, "prior_transform")
  expect_equal(pt$dim, 2)
  expect_equal(pt$names, c("param1", "param2"))
  expect_equal(pt$lb, c(0, 0))
  expect_equal(pt$ub, c(1, 1))
})

test_that("prior_transform.distribution handles missing names", {
  dist1 <- distributional::dist_normal(0, 5)
  dist2 <- distributional::dist_normal(5, 2)

  pt <- prior_transform(dist1, dist2)

  expect_s3_class(pt, "prior_transform")
  expect_equal(pt$dim, 2)
  expect_equal(pt$names, c("X", "X.1"))
})

test_that("prior_transform.function handles missing names", {
  fn <- function(params) {
    c(params[1] * 2, params[2] + 3)
  }

  expect_error(prior_transform(fn = fn, num_dim = 2, .names = NULL))
})
