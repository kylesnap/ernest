test_that("UniformCube function returns correct object", {
  gauss <- make_gaussian(3L)
  sampler <- UniformCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L
  )
  expect_true(inherits(sampler, UniformCube))
  expect_true(inherits(sampler, ErnestLRPS))
})

test_that("Errors can be thrown", {
  gauss <- make_gaussian(3L)

  expect_error(UniformCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L,
    n_points = 0
  ))
  expect_error(UniformCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L,
    first_update = -1
  ))
  expect_error(UniformCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L,
    between_update = 0
  ))
  expect_error(UniformCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L,
    verbose = "apple"
  ))
})
