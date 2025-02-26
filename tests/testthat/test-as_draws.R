test_that("Ernest Sampler Formating Works", {
  skip_if_not_installed("posterior")
  gauss <- make_gaussian(3L)
  sampler <- new_uniform_cube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L,
    n_points = 500L,
    first_update = 1000L,
    between_update = 1000L,
    verbose = FALSE
  )

  expect_error(
    posterior::as_draws(sampler),
    "No iterations have been run with this sampler."
  )

  set.seed(667)
  run <- generate(
    sampler,
    max_it = 100L
  )

  expect_snapshot(posterior::as_draws(run))

  expect_equal(posterior::niterations(posterior::as_draws(run)), 600)
  expect_equal(posterior::niterations(posterior::as_draws(run, resample = FALSE)), 600)
  expect_equal(posterior::niterations(posterior::as_draws(run, unit_scale = TRUE)), 600)

  expect_equal(posterior::niterations(posterior::as_draws(run, add_live = FALSE)), 100)
  expect_equal(posterior::niterations(posterior::as_draws(run, resample = FALSE, add_live = FALSE)), 100)
  expect_equal(posterior::niterations(posterior::as_draws(run, unit_scale = TRUE, add_live = FALSE)), 100)
})
