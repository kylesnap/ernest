test_that("Gaussian case works as expected", {
  gauss_2 <- make_gaussian(2L)
  sampler <- nested_sampling(
    gauss_2$log_lik,
    prior_transform = gauss_2$prior_transform,
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )

  run <- generate(sampler, dlogz = 0.05)
  expected_logz <- 2 * -log(2 * 10)
  integral <- calculate(run, progress = FALSE)

  expect_equal(expected_logz, integral$log_evidence, tolerance = integral$log_evidence_err)

  samples <- posterior::as_draws(run, resample = TRUE)
  sample_sum <- posterior::summarise_draws(samples)
  expect_equal(sample_sum$mean[1], -1, tolerance = sample_sum$sd[1])
  expect_equal(sample_sum$mean[2], 1, tolerance = sample_sum$sd[2])
})
