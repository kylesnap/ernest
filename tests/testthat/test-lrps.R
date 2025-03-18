test_that("unif_cube creates correct structure", {
  sampler <- unif_cube(max_loop = 1000)
  expect_s3_class(sampler, "uniform_cube")
  expect_s3_class(sampler, "ernest_lrps")

  expect_error(
    unif_cube(max_loop = 0),
    "larger than or equal to 1 or `NULL`"
  )
})

test_that("rwmh_cube creates correct structure", {
  sampler <- rwmh_cube(epsilon = 0.2, steps = 30)
  expect_s3_class(sampler, "rw_cube")
  expect_s3_class(sampler, "ernest_lrps")
  expect_equal(sampler$epsilon, list(0.2))
  expect_equal(sampler$num_steps, 30)
  expect_equal(sampler$target_acceptance, 0.5)

  expect_error(
    rwmh_cube(steps = 0),
    "larger than or equal to 2"
  )
  expect_error(
    rwmh_cube(epsilon = 0),
    "larger than zero"
  )
  expect_error(
    rwmh_cube(target_acceptance = 1.5)
  )
  expect_error(
    rwmh_cube(epsilon = 'a'),
    "must be a number"
  )
})
