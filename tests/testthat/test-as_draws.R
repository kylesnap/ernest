test_that("Ernest Sampler Formating Works", {
  gauss <- make_gaussian(3L)
  sampler <- nested_sampling(
    gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L
  )

  expect_error(
    as_draws(sampler),
    "No iterations have been run with this sampler."
  )
  expect_error(
    as_draws(sampler, scale = "bloop")
  )

  set.seed(667)
  run <- generate(
    sampler,
    max_it = 100L
  )

  expect_snapshot(posterior::as_draws(run))

  expect_equal(posterior::niterations(posterior::as_draws(run)), 600)
  expect_equal(posterior::niterations(posterior::as_draws(run, scale = "unit")), 600)
  expect_equal(posterior::niterations(posterior::as_draws_matrix(run)), 600)
  expect_equal(posterior::niterations(posterior::as_draws_df(run)), 600)

  expect_equal(posterior::niterations(posterior::as_draws(run, inc_live = FALSE)), 100)
  expect_equal(posterior::niterations(posterior::as_draws(run, scale = "unit", inc_live = FALSE)), 100)
})
