test_that("Gaussian case works as expected", {
  gauss_2 <- make_gaussian(2L)
  sampler <- nested_sampling(
    gauss_2$log_lik,
    prior_transform = gauss_2$prior_transform,
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )

  run <- generate(sampler, max_it = 1000)
  expect_equal(run$wrk$n_iter, 1000)

  run2 <- generate(run, dlogz = 0.05)
  integral <- calculate(run2, progress = FALSE)

  expected_logz <- 2 * -log(2 * 10)
  expect_equal(expected_logz, integral$log_evidence, tolerance = integral$log_evidence_err)

  samples <- posterior::as_draws(run, resample = TRUE)
  sample_sum <- posterior::summarise_draws(samples)
  expect_equal(sample_sum$mean[1], -1, tolerance = sample_sum$sd[1])
  expect_equal(sample_sum$mean[2], 1, tolerance = sample_sum$sd[2])
})
