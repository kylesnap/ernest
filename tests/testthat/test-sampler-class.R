set.seed(42)
gaussian_2 <- make_gaussian(2)
gaussian_2$log_lik(c(4.56, 1.52))

test_that("ernest_sampler initializes correctly", {
  sampler <- ernest_sampler$new(
    log_lik_fn = gaussian_2$log_lik,
    prior = gaussian_2$prior,
    sampling = rwmh_cube(),
    n_points = 500,
    first_update = 200L,
    update_interval = 50L
  )
  expect_equal(sampler$niterations, 0)
  expect_equal(sampler$ncalls, 0)
  expect_null(sampler$sample_unit)
  expect_null(sampler$sample_log_lik)
  expect_snapshot(sampler)
})

sampler <- ernest_sampler$new(
  log_lik_fn = gaussian_2$log_lik,
  prior = gaussian_2$prior,
  sampling = rwmh_cube(),
  n_points = 500,
  first_update = 200L,
  update_interval = 50L
)

test_that("compile method initializes live points", {
  sampler$compile()
  expect_equal(sampler$niterations, 0)
  expect_equal(sampler$ncalls, 0)
  orig_units <- sampler$live_points$unit
  orig_log_lik <- sampler$live_points$log_lik

  expect_equal(dim(orig_units), c(500, 2))
  expected_log_lik <- apply(
    t(apply(orig_units, 1, gaussian_2$prior$fn)),
    1,
    gaussian_2$log_lik
  )

  expect_equal(orig_log_lik, expected_log_lik)
  expect_snapshot(sampler)
})

test_that("generate method performs sampling", {
  result <- sampler$generate(max_iterations = 99)
  expect_equal(sampler$niterations, 99)
  orig_units <- sampler$live_points$unit
  orig_log_lik <- sampler$live_points$log_lik

  expect_equal(dim(orig_units), c(500, 2))
  expected_log_lik <- apply(
    t(apply(orig_units, 1, gaussian_2$prior$fn)),
    1,
    gaussian_2$log_lik
  )
  expect_snapshot(sampler)
  expect_snapshot(summary(result))
})

test_that("clear method resets sampler state", {
  new_sampler <- ernest_sampler$new(
    log_lik_fn = gaussian_2$log_lik,
    prior = gaussian_2$prior,
    sampling = rwmh_cube(),
    n_points = 500,
    first_update = 200L,
    update_interval = 50L
  )

  sampler$clear()
  expect_equal(as.list(new_sampler), as.list(sampler))
})
