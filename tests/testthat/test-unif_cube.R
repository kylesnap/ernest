test_that("unif_cube function returns correct object", {
  sampler <- unif_cube()
  expect_s3_class(sampler, "unif_cube")
  expect_s3_class(sampler, "ernest_sampler")
})

test_that("unif_cube function handles max_attempts correctly", {
  expect_warning(unif_cube(max_attempts = 500), "max_attempts is less than 1000. This may result in poor sampling.")
  expect_error(unif_cube(max_attempts = 0), "must be a whole number")
})

test_that("update_sampler.unif_cube function refreshes sampler", {
  sampler <- unif_cube(max_attempts = 1234)
  sampler <- update_sampler(sampler, num_dim = 5)
  expect_s3_class(sampler, "unif_cube")
  expect_equal(sampler$num_dim, 5)
  expect_equal(sampler$max_attempts, 1234)
  refreshed_sampler <- refresh_sampler.unif_cube(sampler)
  expect_s3_class(refreshed_sampler, "unif_cube")
  expect_equal(refreshed_sampler$max_attempts, sampler$max_attempts)
})

test_that("Random seeding works for propose_uniform()", {
  flat <- make_flat(2)
  sampler <- unif_cube()
  sampler <- update_sampler(
    sampler,
    log_lik = flat$log_lik,
    prior_transform = flat$prior_transform,
    num_dim = flat$prior_transform$dim
  )

  mat_a <- matrix(nrow = 1000, ncol = 2)
  set.seed(667)
  for (i in 1:1000) {
    mat_a[i, ] <- propose_uniform(sampler, 0)$unit
  }

  mat_b <- matrix(nrow = 1000, ncol = 2)
  set.seed(667)
  for (i in 1:1000) {
    mat_b[i, ] <- propose_uniform(sampler, 0)$unit
  }

  expect_identical(mat_a, mat_b)
  expect_equal(colMeans(mat_a), c(0.5, 0.5), tolerance = 1/12)
  expect_equal(colSums(mat_a < 0), c(0, 0))
  expect_equal(colSums(mat_a > 1), c(0, 0))
})
