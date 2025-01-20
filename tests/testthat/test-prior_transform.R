test_that("set_prior_transform with distribution", {
  dist <- distributions3::Normal(0, 1)
  names <- c("var1", "var2")
  pt <- set_prior_transform(dist, names)
  supp <- unname(distributions3::support(dist))

  expect_s3_class(pt, "prior_transform")
  expect_equal(pt$dim, 2)
  expect_equal(pt$names, names)
  expect_equal(pt$description, rep(as.character(dist), length(names)))
  expect_equal(pt$lb, c(-Inf, -Inf))
  expect_equal(pt$ub, c(Inf, Inf))
})

test_that("set_prior_transform with function", {
  fn <- function(x) x^2
  names <- c("var1", "var2")
  lb <- 0
  ub <- 1
  pt <- set_prior_transform(fn, names, lb, ub)

  expect_s3_class(pt, "prior_transform")
  expect_equal(pt$dim, length(names))
  expect_equal(pt$names, names)
  expect_equal(pt$description, rep("user function", length(names)))
  expect_equal(pt$lb, lb)
  expect_equal(pt$ub, ub)
})

test_that("set_prior_transform with prior_transform", {
  dist <- distributions3::Normal(0, 1)
  names1 <- "var1"
  pt1 <- set_prior_transform(dist, names1)

  fn <- function(x) x^2
  names2 <- "var2"
  pt2 <- set_prior_transform(fn, names2, 0, 1)

  pt_combined <- set_prior_transform(pt1, pt2)

  expect_s3_class(pt_combined, "prior_transform")
  expect_equal(pt_combined$dim, c(1, 1))
  expect_equal(pt_combined$names, c("var1", "var2"))
  expect_equal(pt_combined$description, c(as.character(dist), "user function"))
  expect_equal(pt_combined$lb, c(-Inf, 0))
  expect_equal(pt_combined$ub, c(Inf, 1))

  expect_equal(pt_combined$fn(c(0.5, 0.5)), c(0, 0.25))
  expect_equal(pt_combined$fn(c(0.25, 0.75)), c(qnorm(0.25), 0.75^2))
  expect_equal(pt_combined$fn(c(0.75, 0.25)), c(qnorm(0.75), 0.25^2))
})
