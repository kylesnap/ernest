sampler <- ernest_sampler(
  log_lik = gaussian_blobs$log_lik,
  prior = gaussian_blobs$prior
)

test_that("generate method performs sampling", {
  result <- generate(sampler, max_iterations = 99, seed = 42)
  expect_equal(result$n_iter, 99)
  orig_units <- sampler$run_env$unit
  orig_log_lik <- sampler$run_env$log_lik

  expect_equal(dim(orig_units), c(500, 2))
  expected_log_lik <- apply(
    t(apply(orig_units, 1, gaussian_blobs$prior$fn)),
    1,
    gaussian_blobs$log_lik
  )
  expect_snapshot(result)
  expect_snapshot(summary(result))
})
