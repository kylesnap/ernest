test_that("Plot works as expected", {
  gauss_2 <- make_gaussian(2L)
  sampler <- nested_sampling(
    gauss_2$log_lik,
    prior_transform = gauss_2$prior_transform,
    ptype = 2L,
    n_points = 500L,
    sampler = unif_cube()
  )
  sampler$compile()

  expect_error(
    plot(sampler),
    "No iterations have been run."
  )

  set.seed(420)
  generate(sampler, max_calls = 1000L)
  expect_no_error(plot(sampler))
  #vdiffr::expect_doppelganger("ernest-max_it", plot(sampler))
})
