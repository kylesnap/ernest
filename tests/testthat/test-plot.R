test_that("Plot works as expected", {
  gauss_2 <- make_gaussian(2L)
  sampler <- nested_sampling(
    gauss_2$log_lik,
    prior = gauss_2$prior_transform,
    variables = c(".x", ".y"),
    ptype = 2L,
    n_points = 500L,
    sampler = unif_cube()
  )
  set.seed(420)
  sampler$compile()
  expect_error(
    plot(sampler),
    "No iterations have been run."
  )

  set.seed(420)
  generate(sampler, max_calls = 1000L)
  expect_no_error(plot(sampler))
  expect_no_error(plot(sampler, true_log_z = -5.991465))
  vdiffr::expect_doppelganger(
    "ernest-max_it",
    plot(sampler, true_log_z = -5.991465)
  )
  vdiffr::expect_doppelganger(
    "ernest-no_exp",
    plot(sampler, exponentiate = FALSE)
  )
})
