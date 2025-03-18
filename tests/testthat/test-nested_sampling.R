test_that("nested_sampling function works correctly", {
  result <- nested_sampling(
    gaussian_2$log_lik,
    gaussian_2$prior_transform,
    ptype = c("A", "B"),
    n_points = 500,
    sampler = rwmh_cube(),
    update_interval = 1.5,
    verbose = TRUE
  )

  expect_s3_class(result, "ernest_sampler")
  expect_snapshot(result)
})

test_that("nested_sampling.default throws errors", {
  expect_error(nested_sampling(1), "No method defined for")
  log_lik <- gaussian_2$log_lik
  prior_trans <- gaussian_2$prior_transform
  expect_warning(
    nested_sampling(log_lik, prior_trans, ptype = "n_dim"),
    "specifiying a uni-dimensional"
  )
  expect_error(
    nested_sampling(log_lik, prior_trans, ptype = 2, n_points = "500"),
    "must be a whole number"
  )
  expect_error(
    nested_sampling(log_lik, prior_trans, ptype = 2, sampler = list()),
    "sampler must be an ernest_lrps object"
  )
})

test_that("Default parameters behave as expected", {
  result <- nested_sampling(
    gaussian_2$log_lik,
    gaussian_2$prior_transform,
    2
  )

  expect_s3_class(result, c("ernest_sampler", "R6"))
  expect_snapshot(result)
})
