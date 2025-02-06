test_that("2D Gaussian Likelihood (Uniform Cube)", {
  gauss <- make_gaussian(2)

  result <- nested_sampling(
    gauss$log_lik,
    prior = gauss$prior_transform,
    max_iter = 2000,
    sampler = unif_cube()
  )

  log_z <- tail(result$integration$log_z, 1)
  log_z_err <- sqrt(tail(result$integration$log_z_var, 1))
  expect_lt(abs(log_z - gauss$logz_truth), 3.0 * log_z_err)
  expect_equal(sum(exp(result$samples$.log_weight)), 1.0)

  result <- nested_sampling(
    gauss$log_lik,
    prior = gauss$prior_transform,
    sampler = rw_cube()
  )

  log_z <- tail(result$integration$log_z, 1)
  log_z_err <- sqrt(tail(result$integration$log_z_var, 1))
  expect_lt(abs(log_z - gauss$logz_truth), 3.0 * log_z_err)
  expect_equal(sum(exp(result$samples$.log_weight)), 1.0)
  summary(result) |> print()
})
