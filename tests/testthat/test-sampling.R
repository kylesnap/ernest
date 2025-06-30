test_that("unif_cube creates correct structure", {
  sampler <- unif_cube()
  expect_s3_class(sampler, "ernest_sampling")
  expect_equal(sampler$name, "Uniform Cube")
  expect_equal(sampler$parameters, list())
  expect_equal(sampler$class, uniform_lrps)
})

test_that("rwmh_cube creates correct structure", {
  sampler <- rwmh_cube()
  expect_s3_class(sampler, "ernest_sampling")
  expect_equal(sampler$name, "Random Walk Cube")
  expect_equal(sampler$parameters, list(steps = 25L, target_acceptance = 0.5))
  expect_equal(sampler$class, rwcube_lrps)
})
