library(mvtnorm)
library(distributional)

test_that("Call nested_sampling with a function and function prior", {
  gaussian_2_ll <- \(x) {
    sum(dmvnorm(x, mean = c(-1, 1), sigma = diag(2), log = TRUE))
  }
  prior_2 <- \(p) qunif(p, -5, 5)

  ns <- nested_sampling(
    gaussian_2_ll,
    prior_2,
    names = c("x1", "x2")
  )
  compile(ns)

  expect_snapshot(ns)
  expect_equal(
    as.matrix(ns$get_live_points()),
    t(apply(ns$get_live_points("unit"), 1, prior_2))
  )
})

test_that("Call nested_sampling with a function and distribution prior", {
  gaussian_2_ll <- \(x) {
    sum(dmvnorm(x, mean = c(-1, 1), sigma = diag(2), log = TRUE))
  }
  prior_2 <- c(dist_uniform(-5, 5), dist_uniform(-5, 5))

  ns <- nested_sampling(
    gaussian_2_ll,
    prior_2,
    names = c("x1", "x2")
  )
  compile(ns)

  expect_snapshot(ns)
  expect_equal(
    as.matrix(ns$get_live_points()),
    t(apply(ns$get_live_points("unit"), 1, \(x) qunif(x, -5, 5)))
  )
})

test_that("Call nested_sampling with a glm and function prior", {
  model <- glm(
    mpg ~ wt + hp,
    data = mtcars,
    family = gaussian()
  )
  prior <- \(p) {
    c(
      qnorm(p[1:3], 0, 1),
      quantile(dist_truncated(dist_cauchy(0, 25), lower = 0), p[4])
    )
  }

  ns <- nested_sampling(
    model,
    prior,
    names = c("(Intercept)", "wt", "hp", "sigma"),
  )
  compile(ns)

  expect_snapshot(ns)
  expect_equal(
    unname(as.matrix(ns$get_live_points("original"))),
    unname(t(apply(ns$get_live_points("unit"), 1, prior)))
  )
})

test_that("call nested_sampling with glm and default priors", {
  model <- glm(
    mpg ~ wt + hp,
    data = mtcars,
    family = gaussian()
  )
  prior <- \(p) {
    c(
      qnorm(p[1:3], 0, 1),
      quantile(dist_truncated(dist_cauchy(0, 25), lower = 0), p[4])
    )
  }

  ns <- nested_sampling(model)
  compile(ns)

  expect_snapshot(ns)
  expect_equal(
    unname(as.matrix(ns$get_live_points("original"))),
    unname(t(apply(ns$get_live_points("unit"), 1, prior)))
  )
})
