prior <- ernest_prior(gaussian_2$prior_transform)
prior_fn <- compile(
  ernest_prior(gaussian_2$prior_transform)
)

sampler <- ernest_sampler$new(
  log_lik_fn = gaussian_2$log_lik,
  prior = prior,
  sampler = rwmh_cube(),
  n_points = 500,
  first_update = 200L,
  update_interval = 50L
)

test_that("Summary on an empty object", {
  summary <- summary(sampler)

  expect_equal(summary$n_points, 500L)
  expect_equal(summary$n_iterations, 0L)
  expect_equal(summary$n_calls, 0L)
})

compile(sampler)

test_that("Summarise a compiled, but empty sampler object", {
  summary <- summary(sampler)
  expect_equal(summary$n_points, 500L)
  expect_equal(summary$n_iterations, 0L)
  expect_equal(summary$n_calls, 0L)
})

set.seed(500)
run <- generate(sampler, max_it = 1000L)

test_that("Summarise a run with results", {
  summary <- summary(sampler)
  calc <- sampler$calculate()
  expect_equal(summary$n_points, 500L)
  expect_equal(summary$n_iterations, 1000L)
  expect_equal(summary$n_calls, 25000L) # That is, (25 calls * 1000 steps)
  expect_equal(summary$log_evidence, calc$log_evidence)
  expect_equal(summary$log_evidence.sd, sqrt(calc$log_evidence.var))
  expect_equal(summary$information, calc$information)
})
