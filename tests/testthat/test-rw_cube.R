test_that("rw_cube function returns correct object", {
  sampler <- rw_cube()
  expect_s3_class(sampler, "rw_cube")
  expect_s3_class(sampler, "ernest_sampler")
})

test_that("rw_cube function handles max_attempts correctly", {
  expect_error(rw_cube(steps = 0), "`steps` must be a whole number larger than or equal to 1, not the number 0.")
  expect_error(rw_cube(epsilon = 0))
})

test_that("update_sampler.rw_cube function refreshes sampler", {
  sampler <- rw_cube()
  sampler <- update_sampler(sampler, num_dim = 5)
  expect_s3_class(sampler, "rw_cube")
  expect_equal(sampler$num_dim, 5)
  expect_equal(sampler$steps, 20)
  refreshed_sampler <- refresh_sampler.rw_cube(sampler)
  expect_s3_class(refreshed_sampler, "rw_cube")
  expect_equal(refreshed_sampler$epsilon, sampler$epsilon)
})

test_that("Permuter messes with one row in live_u", {
  gauss <- make_gaussian(2)
  sampler <- update_sampler(
    rw_cube(),
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    num_dim = gauss$prior_transform$dim
  )

  env <- new_environment(
    list(live_u = matrix(c(0.35, 0.65, 0.40, 0.60, 0.45, 0.55), nrow = 3, byrow = TRUE))
  )
  cpy <- env_clone(env)

  new <- propose_uniform(sampler, -50)
  expect_gte(new$log_lik, -50)
  expect_equal(env$live_u, cpy$live_u)

  new <- propose_live(sampler, env$live_u[2,], -20.67)
  expect_gte(new$log_lik, -20.67)
  expect_equal(env$live_u, cpy$live_u)

  new <- propose_live(sampler, env$live_u[3,], -0.67)
  expect_gte(new$log_lik, sampler$log_lik(sampler$log_lik(c(-1, 1))))
  expect_equal(env$live_u, cpy$live_u)
})

test_that("Random seeding works for propose_live()", {
  flat <- make_flat(2)
  sampler <- rw_cube()
  sampler <- update_sampler(
    sampler,
    log_lik = flat$log_lik,
    prior_transform = flat$prior_transform,
    num_dim = flat$prior_transform$dim
  )
  seed_values <- matrix(runif(1000 * 2), nrow = 1000, ncol = 2)

  mat_a <- matrix(nrow = 100, ncol = 2)
  set.seed(667)
  for (i in 1:100) {
    mat_a[i, ] <- propose_live(sampler, seed_values[i,], 0)$unit
  }

  mat_b <- matrix(nrow = 100, ncol = 2)
  set.seed(667)
  for (i in 1:100) {
    mat_b[i, ] <- propose_live(sampler, seed_values[i,], 0)$unit
  }

  expect_identical(mat_a, mat_b)
  expect_equal(colMeans(mat_a), c(0.5, 0.5), tolerance = 1/12)
  expect_equal(colSums(mat_a < 0), c(0, 0))
  expect_equal(colSums(mat_a > 1), c(0, 0))
})
