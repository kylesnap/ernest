test_that("unif_cube creates correct structure", {
  sampler <- unif_cube(max_loop = 1000)
  expect_s3_class(sampler, "unif_cube")
  expect_s3_class(sampler, "ernest_lrps")
  expect_equal(sampler$max_loop, 1000)

  expect_error(
    unif_cube(max_loop = 0),
    "larger than or equal to 1 or `NULL`"
  )
})

test_that("rwmh_cube creates correct structure", {
  sampler <- rwmh_cube(epsilon = 0.2, steps = 30, max_loop = 1000)
  expect_s3_class(sampler, "rwmh_cube")
  expect_s3_class(sampler, "ernest_lrps")
  expect_equal(sampler$epsilon, 0.2)
  expect_equal(sampler$steps, 30)
  expect_equal(sampler$max_loop, 1000)

  expect_error(
    rwmh_cube(steps = 0),
    "larger than or equal to 2"
  )
  expect_error(
    rwmh_cube(epsilon = 0),
    "larger than 0"
  )
  expect_error(
    rwmh_cube(epsilon = 'a'),
    "must be a number"
  )
})

test_that("default values carry through", {
  expect_mapequal(unif_cube(), list(
    max_loop = NULL
  ))
  expect_mapequal(rwmh_cube(), list(
    epsilon = 1.0,
    steps = 20,
    p_acc = 0.5,
    max_loop = NULL
  ))
})

test_that("build_sampler.unif_cube creates correct sampler", {
  sampler <- unif_cube(max_loop = 1000)
  result <- build_sampler(
    sampler,
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2,
    n_points = 100,
    first_update = 10,
    between_update = 5,
    verbose = TRUE
  )
  expect_s3_class(result, "UniformCube")
})

test_that("build_sampler.rwmh_cube creates correct sampler", {
  sampler <- rwmh_cube(epsilon = 0.2, steps = 30, max_loop = 1000)
  result <- build_sampler(
    sampler,
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2,
    n_points = 100,
    first_update = 10,
    between_update = 5,
    verbose = TRUE
  )
  expect_s3_class(result, "RandomWalkCube")
})
