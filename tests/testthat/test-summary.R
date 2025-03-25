lrps <- new_rwmh_cube(
  log_lik = gaussian_2$log_lik,
  prior_transform = gaussian_2$prior_transform,
  n_dim = 2,
  update_interval = 200,
  num_steps = 20,
  target_acceptance = 0.25,
  epsilon = 0.1
)

sampler <- ernest_sampler$new(
  lrps = lrps,
  ptype = c("A", "B"),
  n_points = 500
)

test_that("Summary on an empty object", {
  expect_message(
    summary <- summary(sampler),
    "No iterations"
  )

  expect_mapequal(
    summary,
    list(
      n_points = 500L,
      n_iterations = 0L,
      n_calls = 0L
    )
  )

  expect_snapshot(summary)
})

compile(sampler)

test_that("Summarise a compiled, but empty sampler object", {
  expect_message(
    summary <- summary(sampler),
    "No iterations"
  )

  expect_mapequal(
    summary,
    list(
      n_points = 500L,
      n_iterations = 0L,
      n_calls = 0L
    )
  )

  expect_snapshot(summary)
})

set.seed(500)
run <- generate(sampler, max_it = 1000L)

test_that("Summarise a run with results", {
  calc <- calculate(sampler)

  expect_mapequal(
    summary(sampler),
    list(
      n_points = 500L,
      n_iterations = 1000L,
      n_calls = 20000L,
      eff = 0.05,
      log_weight = calc$log_weight,
      log_lik = calc$log_lik,
      log_vol = calc$log_vol,
      log_z = calc$log_z,
      log_z_err = sqrt(calc$log_z_var),
      information = calc$information
    )
  )

  expect_snapshot(summary(sampler))
})
