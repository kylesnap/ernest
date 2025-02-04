test_that("2D Gaussian Likelihood", {
  gauss <- make_gaussian(2)

  result <- nested_sampling(
    gauss$log_lik,
    prior = gauss$prior_transform,
  )

  log_z <- tail(result$integration$log_z, 1)
  log_z_err <- sqrt(tail(result$integration$log_z_var, 1))
  expect_equal(log_z, gauss$logz_truth, tolerance = log_z_err)
})

test_that("Range of Dimensions", {
  for (num_dim in seq_len(6)) {
    gauss <- make_gaussian(num_dim)

    result <- nested_sampling(
      gauss$log_lik,
      prior = gauss$prior_transform,
    )

    log_z <- tail(result$integration$log_z, 1)
    log_z_err <- sqrt(tail(result$integration$log_z_var, 1))
    expect_lt(abs(log_z - gauss$logz_truth), 4 * log_z_err)
  }
})
