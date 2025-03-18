test_that("Gaussian case works as expected", {
  gauss_2 <- make_gaussian(2L)
  sampler <- nested_sampling(
    gauss_2$log_lik,
    prior_transform = gauss_2$prior_transform,
    ptype = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )

  compile(sampler)
  run <- generate(sampler, max_it = 1000)
  expect_equal(run$n_iterations, 1000)

  generate(sampler, min_logz = 0.05)
  glanced <- sampler$glance()
  glanced <- glanced[nrow(glanced),]

  expected_logz <- 2 * -log(2 * 10)
  expect_equal(expected_logz, glanced$log_z, tolerance = glanced$log_z_err)
})
