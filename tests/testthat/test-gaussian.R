test_that("2D Gaussian Likelihood (Uniform Cube)", {
  gauss <- make_gaussian(3L)
  sampler <- UniformCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L
  )
  run <- generate(sampler, max_it = 1000)
  expect_equal(run@n_iter, 1000)

  # sampler <- RandomWalkCube(
  #   log_lik = gauss$log_lik,
  #   prior_transform = gauss$prior_transform,
  #   n_dim = 3L
  # )
  # generate(sampler, max_it = 1000)
})
