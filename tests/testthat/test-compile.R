test_that("ErnestSampler can start from scratch", {
  sampler <- nested_sampling(
    gaussian_2$log_lik,
    prior_transform = gaussian_2$log_lik,
    n_dim = 2,
    verbose = TRUE
  )

  sampler <- compile(sampler)
  expect_equal(dim(sampler$wrk$live_points), c(500, 2))
  expect_equal(dim(sampler$wrk$live_units), c(500, 2))
  expect_equal(length(sampler$wrk$live_lik), 500)
})

test_that("ErnestSampler can start from an existing run", {
  sampler <- nested_sampling(
    gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube(),
    verbose = TRUE
  )
  run <- generate(sampler, max_it = 1000)

  expect_message(
    sampler2 <- compile(run, refresh = FALSE),
    "Run will start from 1000 iterations."
  )
  expect_equal(sampler2$wrk$live_units, run$wrk$live_units)
  expect_equal(sampler2$wrk$live_points, run$wrk$live_points)
  expect_equal(sampler2$wrk$live_lik, run$wrk$live_lik)
  expect_false(obj_address(sampler2$wrk) == obj_address(run$wrk))
})

test_that("ErnestSampler can refresh the workspace", {
    sampler <- nested_sampling(
      gaussian_2$log_lik,
      prior_transform = gaussian_2$prior_transform,
      n_dim = 2L,
      n_points = 500L,
      sampler = rwmh_cube(),
      verbose = TRUE
    )
    run <- generate(sampler, max_it = 1000)

    expect_message(
      sampler2 <- compile(run, refresh = TRUE),
      "Run will start from 0 iterations"
    )
    expect_equal(dim(sampler2$wrk$live_points), c(500, 2))
    expect_equal(dim(sampler2$wrk$live_units), c(500, 2))
    expect_equal(length(sampler2$wrk$live_lik), 500)
    expect_false(obj_address(sampler2$wrk) == obj_address(run$wrk))
  }
)

test_that("ErnestSampler detects errors", {
    sampler <- nested_sampling(
      gaussian_2$log_lik,
      prior_transform = gaussian_2$prior_transform,
      n_dim = 2L,
      n_points = 500L,
      sampler = rwmh_cube()
    )
    run <- generate(sampler, max_it = 1000)

    sampler2 <- compile(run, refresh = FALSE)
    expect_equal(sampler2$wrk$live_units, run$wrk$live_units)
    expect_equal(sampler2$wrk$live_points, run$wrk$live_points)
    expect_equal(sampler2$wrk$live_lik, run$wrk$live_lik)
    expect_false(obj_address(sampler2$wrk) == obj_address(run$wrk))
  }
)

test_that("validate_wrk detects non-finite values in live points", {
  sampler <- nested_sampling(
    gaussian_2$log_lik,
    prior_transform = \(x) if (any(x < 0.5)) c(NaN, NaN) else gaussian_2$prior_transform(x),
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )
  expect_error(compile(sampler), "Non-finite values found in the live points")

  sampler <- nested_sampling(
    gaussian_2$log_lik,
    prior_transform = \(x) if (any(x < 0.5)) c(Inf, -Inf) else gaussian_2$prior_transform(x),
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )
  expect_error(compile(sampler), "Non-finite values found in the live points")

  sampler <- nested_sampling(
    gaussian_2$log_lik,
    prior_transform = \(x) if (any(x < 0.5)) c(NA, 1.0) else gaussian_2$prior_transform(x),
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )
  expect_error(compile(sampler), "Non-finite values found in the live points")
})

test_that("validate_wrk detects NaN values in live likelihoods", {
  sampler <- nested_sampling(
    x = \(x) if (any(x < 0)) NaN else gaussian_2$log_lik(x),
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )
  expect_error(compile(sampler), "Generated missing log-likelihood values")
})

test_that("validate_wrk detects infinite values in live likelihoods", {
  sampler <- nested_sampling(
    \(x) if (any(x < 0)) Inf else gaussian_2$log_lik(x),
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )
  expect_error(compile(sampler), "Generated `Inf` log-likelihood values.")
  sampler <- nested_sampling(
    \(x) if (any(x < 0)) -Inf else gaussian_2$log_lik(x),
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )
  expect_warning(compile(sampler), "Generated `-Inf` log-likelihood values.")
})

test_that("validate_wrk detects identical log-likelihood values", {
  sampler <- nested_sampling(
    \(x) 0.0,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )
  expect_warning(compile(sampler), "All proposed points have the same log-likelihood value.")
})

test_that("validate_wrk detects poor workspaces", {
  sampler <- nested_sampling(
    gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2L,
    n_points = 500L,
    sampler = rwmh_cube()
  )
  run <- generate(sampler, max_it = 1000)

  copy <- run$wrk$clone(deep = TRUE)

  run$wrk$live_points <- copy$live_points[1:499, ]
  expect_error(compile(run), "The workspace is malconfigured")
  run$wrk$live_points <- copy$live_points
  run$wrk$live_units <- copy$live_units[1:499, ]
  expect_error(compile(run), "The workspace is malconfigured")
  run$wrk$live_units <- copy$live_units
  run$wrk$live_lik <- copy$live_lik[1:499]
  expect_error(compile(run), "The workspace is malconfigured")
  run$wrk$live_lik <- copy$live_lik

  run$n_points <- 499L
  expect_error(compile(run), "The workspace is malconfigured")
})
