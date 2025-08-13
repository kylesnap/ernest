gaussian_2 <- make_gaussian(2)

test_that("ernest_sampler initializes correctly", {
  sampler <- new_ernest_sampler(
    log_lik = gaussian_2$log_lik,
    prior = gaussian_2$prior,
    sampling = rwmh_cube(),
    n_points = 500,
    first_update = 200L,
    update_interval = 50L
  )

  expect_identical(sampler$log_lik, gaussian_2$log_lik)
  expect_identical(sampler$prior, gaussian_2$prior)
  expect_identical(sampler$n_points, 500L)
  expect_identical(sampler$first_update, 200L)
  expect_identical(sampler$update_interval, 50L)
  expect_identical(env_depth(sampler$live_points), 1L)
  expect_snapshot(sampler)
})

sampler_call <- call2(
  new_ernest_sampler,
  log_lik_fn = gaussian_2$log_lik,
  prior = gaussian_2$prior,
  sampling = rwmh_cube(),
  n_points = 500,
  first_update = 200L,
  update_interval = 50L
)
test_that("invalid samplers are caught", {
  expect_no_error(bad_sampler <- eval(sampler_call))

  # Invalid Points
  points_call <- call_modify(sampler_call, n_points = 0L)
  expect_snapshot_error(eval(points_call))
  bad_sampler$n_points <- Inf
  expect_snapshot_error(check_ernest_sampler(bad_sampler))

  # Invalid first update
  first_update_call <- call_modify(sampler_call, first_update = -1L)
  expect_snapshot_error(eval(first_update_call))
  bad_sampler$first_update <- Inf
  expect_snapshot_error(check_ernest_sampler(bad_sampler))

  # Invalid update interval
  update_interval_call <- call_modify(sampler_call, update_interval = -1L)
  expect_snapshot_error(eval(update_interval_call))
  bad_sampler$update_interval <- Inf
  expect_snapshot_error(check_ernest_sampler(bad_sampler))

  # Invalid log_lik_fn
  loglik_call <- call_modify(sampler_call, log_lik_fn = list())
  expect_snapshot_error(eval(loglik_call))
  bad_sampler$log_lik_fn <- "sum"
  expect_snapshot_error(check_ernest_sampler(bad_sampler))

  # Invalid prior
  prior_call <- call_modify(sampler_call, prior = list())
  expect_snapshot_error(eval(prior_call))
  bad_sampler$prior <- "sum"
  expect_snapshot_error(check_ernest_sampler(bad_sampler))

  # Invalid sampling/LRPS
  sampling_call <- call_modify(sampler_call, sampling = list())
  expect_snapshot_error(eval(sampling_call))
  bad_sampler$lrps <- list()
  expect_snapshot_error(check_ernest_sampler(bad_sampler))
})

test_that("refresh works as expected", {
  sampler <- eval(sampler_call)
  sampler2 <- refresh_ernest_sampler(sampler)
  expect_identical(sampler, sampler2)
})
