gaussian_2 <- make_gaussian(2)
sampler <- new_ernest_sampler(
  log_lik_fn = gaussian_2$log_lik,
  prior = gaussian_2$prior,
  sampling = rwmh_cube(),
  n_points = 500,
  first_update = 200L,
  update_interval = 50L
)

test_that("generate method performs sampling", {
  result <- generate(sampler, max_iterations = 99, seed = 42)
  expect_equal(result$n_iter, 99)
  orig_units <- sampler$live_points$unit
  orig_log_lik <- sampler$live_points$log_lik

  expect_equal(dim(orig_units), c(500, 2))
  expected_log_lik <- apply(
    t(apply(orig_units, 1, gaussian_2$prior$fn)),
    1,
    gaussian_2$log_lik
  )
  expect_snapshot(result)
  expect_snapshot(summary(result))
})
