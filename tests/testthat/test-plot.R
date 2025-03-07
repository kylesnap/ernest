test_that("Plot works as expected", {
  gauss_2 <- make_gaussian(2L)
  sampler <- nested_sampling(
    gauss_2$log_lik,
    prior_transform = gauss_2$prior_transform,
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )

  expect_error(
    plot(sampler),
    "No evidence has been calculated"
  )

  set.seed(420)
  run <- generate(sampler, max_it = 1000)
  vdiffr::expect_doppelganger("ernest-1k", plot(run))

  set.seed(420)
  run <- generate(sampler, dlogz = 0.5)
  vdiffr::expect_doppelganger("ernest-dlogz", plot(run))
})
