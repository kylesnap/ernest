gaussian_2 <- make_gaussian(2)
sampler <- ernest_sampler$new(
  log_lik_fn = gaussian_2$log_lik,
  prior = gaussian_2$prior,
  sampling = rwmh_cube(),
  n_points = 500,
  first_update = 200L,
  update_interval = 50L
)

set.seed(42L)
result <- generate(sampler, max_iterations = 100L)
test_that("ernest_result returns as expected", {
  expect_s3_class(result, "ernest_run")
  expect_equal(sort(unique(result$id)), seq(1, 500))
  expect_equal(result$points, c(rep(500L, 100), seq(500, 1, -1)))
  expect_equal(sort(unique(result$birth)), seq(0, 100))

  row_match <- matrix(double(600L * 2L), nrow = 600L)
  logl_match <- double(600L)
  for (i in seq_len(nrow(result$samples))) {
    row_match[i, ] <- gaussian_2$prior$fn(result$samples_unit[i, ])
    logl_match[i] <- gaussian_2$log_lik(row_match[i, ])
  }
  colnames(row_match) <- gaussian_2$prior$varnames
  expect_equal(row_match, result$samples)
  expect_equal(logl_match, result$log_lik)
  expect_equal(sort(attr(result, "live_loc")), seq(101, 600))
  expect_snapshot(result)
})

set.seed(42L)
result2 <- generate(sampler, max_iterations = 300L)
test_that("Runs can continue after one call", {
  expect_equal(result2$n_iter, 300L)

  first <- seq(100)
  expect_equal(result2$samples[first, ], result$samples[first, ])
  expect_equal(result2$log_lik[first], result$log_lik[first])
  expect_equal(result2$points[first], result$points[first])
  expect_equal(result2$calls[first], result$calls[first])
  expect_equal(result2$birth[first], result$birth[first])
  expect_equal(result2$id[first], result$id[first])
  expect_snapshot(result2)
})

set.seed(42L)
result3 <- generate(sampler, max_iterations = 1000L)
test_that("Runs can continue after two calls", {
  expect_equal(result3$n_iter, 1000L)

  first <- seq(300)
  expect_equal(result3$samples[first, ], result2$samples[first, ])
  expect_equal(result3$log_lik[first], result2$log_lik[first])
  expect_equal(result3$points[first], result2$points[first])
  expect_equal(result3$calls[first], result2$calls[first])
  expect_equal(result3$birth[first], result2$birth[first])
  expect_equal(result3$id[first], result2$id[first])
  expect_snapshot(result3)
})

test_that("Summary method returns", {
  expect_snapshot(summary(result3))
})
