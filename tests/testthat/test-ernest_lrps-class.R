library(mvtnorm)
library(distributional)

n_dim <- 3
sigma <- diag(n_dim)
sigma[sigma == 0] <- 0.4
log_lik_fn <- \(x) {
  dmvnorm(x, mean = seq(-1, 1, length.out = n_dim), sigma = sigma, log = TRUE)
}
prior_fn <- \(x) qunif(x, -5, 5)
prior_fn2 <- ernest_prior(c(
  "a" = dist_uniform(-5, 5),
  "b" = dist_uniform(-5, 5),
  "c" = dist_uniform(-5, 5)
))

test_that("ernest_lrps class initializes correctly", {
  lrps <- ernest_lrps$new(log_lik_fn, prior_fn, n_dim)

  expect_s3_class(lrps, "ernest_lrps")
  expect_mapequal(
    lrps$history,
    tibble::tibble(
      n_iter = integer(0L),
      n_call = integer(0L)
    )
  )

  lrps <- ernest_lrps$new(log_lik_fn, compile(prior_fn2), n_dim)
  expect_s3_class(lrps, "ernest_lrps")
  expect_mapequal(
    lrps$history,
    tibble::tibble(
      n_iter = integer(0L),
      n_call = integer(0L)
    )
  )
})

test_that("Uniform subclass initializes and clears correctly", {
  uniform <- uniform_lrps$new(log_lik_fn, prior_fn, n_dim)

  expect_s3_class(uniform, c("uniform_sampler", "ernest_lrps"))
  expect_mapequal(
    uniform$history,
    tibble::tibble(
      n_iter = integer(0L),
      n_call = integer(0L)
    )
  )

  uniform$clear()
  expect_s3_class(uniform, c("uniform_sampler", "ernest_lrps"))
  expect_mapequal(
    uniform$history,
    tibble::tibble(
      n_iter = integer(0L),
      n_call = integer(0L)
    )
  )
})

uniform <- uniform_lrps$new(log_lik_fn, prior_fn, n_dim)
test_that("Uniform subclass can propose_uniform", {
  new <- uniform$propose_uniform(criterion = -Inf)
  expect_named(new, c("unit", "parameter", "log_lik", "num_calls"))
  expect_length(new$unit, n_dim)
  expect_equal(new$parameter, prior_fn(new$unit))
  expect_gt(new$log_lik, -Inf)
  expect_equal(new$num_calls, 1L)

  for (i in seq(10)) {
    new <- uniform$propose_uniform(criterion = -4.74)
    expect_gt(new$log_lik, -4.74)
  }

  new <- uniform$propose_live(c(-0.4, -1.5, 1.2), criterion = -4.74)
  expect_gt(new$log_lik, -4.74)
})

test_that("Uniform logs correctly after update", {
  uniform$update()
  expect_named(
    {
      hist <- uniform$history
    },
    c("n_iter", "n_call")
  )
  expect_equal(hist$n_iter, 12L)
  expect_gt(hist$n_call, 1L)
})

test_that("rwcube subclass initializes and clears correctly", {
  rw <- rwcube_lrps$new(
    log_lik_fn,
    prior_fn,
    n_dim,
    epsilon = 0.5
  )

  expect_s3_class(rw, c("uniform_sampler", "ernest_lrps"))
  expect_mapequal(
    rw$history,
    tibble::tibble(
      n_iter = integer(0L),
      n_call = integer(0L),
      epsilon = numeric(0L),
      acc_ratio = numeric(0L)
    )
  )
  expect_equal(rw$epsilon, 0.5)

  rw$clear()
  expect_s3_class(rw, c("uniform_sampler", "ernest_lrps"))
  expect_mapequal(
    rw$history,
    tibble::tibble(
      n_iter = integer(0L),
      n_call = integer(0L),
      epsilon = numeric(0L),
      acc_ratio = numeric(0L)
    )
  )
  expect_equal(rw$epsilon, 0.5)
})

rw <- rwcube_lrps$new(log_lik_fn, prior_fn, n_dim)
test_that("rw subclass can propose_uniform", {
  new <- rw$propose_uniform(criterion = -Inf)
  expect_named(new, c("unit", "parameter", "log_lik", "num_calls"))
  expect_length(new$unit, n_dim)
  expect_equal(new$parameter, prior_fn(new$unit))
  expect_gt(new$log_lik, -Inf)
  expect_equal(new$num_calls, 1L)

  for (i in seq(10)) {
    new <- rw$propose_uniform(criterion = -4.74)
    expect_gt(new$log_lik, -4.74)
  }
})

test_that("rw subclass can propose_live", {
  new <- rw$propose_live(c(0.46, 0.35, 0.62), criterion = -Inf)
  expect_named(new, c("unit", "parameter", "log_lik", "num_calls", "num_acc"))
  expect_length(new$unit, n_dim)
  expect_equal(new$parameter, prior_fn(new$unit))
  expect_gt(new$log_lik, -Inf)
  expect_equal(new$num_calls, 20L)
  expect_lte(new$num_acc, 20L)

  new <- rw$propose_live(c(0.46, 0.35, 0.62), criterion = -4.74)
  expect_gte(new$log_lik, -4.74)

  # Failsafe: Criterion impossible to reach
  new <- rw$propose_live(c(0.46, 0.35, 0.62), criterion = 0)
  expect_equal(new$unit, c(0.46, 0.35, 0.62))
  expect_equal(new$num_acc, 0)
})


test_that("rw update", {
  rw$clear()
  test_mat <- matrix(runif(100 * 3), nrow = 100, ncol = 3)
  apply(
    test_mat,
    1,
    \(x) rw$propose_live(x, criterion = log_lik_fn(prior_fn(x)))
  )
  rw$update()
  expect_named(rw$history, c("n_iter", "n_call", "epsilon", "acc_ratio"))
  expect_equal(rw$history$n_iter, 100L)
  expect_equal(rw$history$n_call, 2000L)
  expect_equal(rw$history$epsilon, 1)
  expect_lt(rw$history$acc_ratio, 1)
  cur_eps <- rw$epsilon

  apply(
    test_mat,
    1,
    \(x) rw$propose_live(x, criterion = log_lik_fn(prior_fn(x)))
  )
  rw$update()
  expect_equal(rw$history$n_iter, c(100, 200))
  expect_equal(rw$history$n_call, c(2000, 4000))
  expect_equal(rw$history$epsilon, c(1, cur_eps))
  expect_gt(
    (rw$history$acc_ratio[1] - 0.5)^2,
    (rw$history$acc_ratio[2] - 0.5)^2
  )
})
