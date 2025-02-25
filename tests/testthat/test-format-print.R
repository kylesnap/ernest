test_that("Ernest Sampler Formating Works", {
  gauss <- make_gaussian(3L)
  sampler <- UniformCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L
  )
  expect_snapshot(sampler)

  set.seed(667)
  run <- generate(
    sampler,
    max_it = 100L
  )
  expect_snapshot(run)

  sampler <- RandomWalkCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L
  )
  expect_snapshot(sampler)
})
