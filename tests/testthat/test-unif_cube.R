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

test_that("Permuter messes with one row in live_u", {
  gauss <- make_gaussian(2)
  sampler <- update_sampler(
    unif_cube(),
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    num_dim = gauss$prior_transform$dim
  )

  env <- new_environment(
    list(live_u = matrix(c(0.35, 0.65, 0.40, 0.60), nrow = 2, byrow = TRUE))
  )
  cpy <- env_clone(env)

  new <- propose_uniform(sampler, -50)
  expect_gte(new$log_lik, -50)
  expect_equal(env$live_u, cpy$live_u)

  new <- propose_live(sampler, env$live_u[2,], -20.67)
  expect_gte(new$log_lik, -20.67)
  expect_equal(env$live_u, cpy$live_u)

  # new <- propose_live(sampler, env$live_u[2,], -50)
  # expect_gte(new$log_lik, -50)
  # expect_equal(env$live_u, cpy$live_u)
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
