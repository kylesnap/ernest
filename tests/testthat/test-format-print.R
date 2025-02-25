test_that("Ernest Sampler Formating Works", {
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
  expect_snapshot(sampler)

  set.seed(667)
  run <- generate(
    sampler,
    max_it = 100L
  )
  expect_snapshot(run)

  sampler <- new_rwmh_cube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L,
    n_points = 500L,
    first_update = 1000L,
    between_update = 1000L,
    steps = 20L,
    epsilon = 0.1,
    verbose = FALSE
  )
  expect_snapshot(sampler)
})
