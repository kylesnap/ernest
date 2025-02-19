test_that("Ernest Sampler Formating Works", {
  gauss <- make_gaussian(3L)
  sampler <- UniformCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L
  )
  run <- generate(
    sampler,
    max_it = 100L
  )

  expect_equal(posterior::niterations(as_draws(run)), 600)
  expect_equal(posterior::niterations(as_draws(run, resample = FALSE)), 600)
  expect_equal(posterior::niterations(as_draws(run, unit_scale = TRUE)), 600)

  expect_equal(posterior::niterations(as_draws(run, add_live = FALSE)), 100)
  expect_equal(posterior::niterations(as_draws(run, resample = FALSE, add_live = FALSE)), 100)
  expect_equal(posterior::niterations(as_draws(run, unit_scale = TRUE, add_live = FALSE)), 100)
})
