test_that("Tidy output", {
  gauss <- make_gaussian(3L)
  sampler <- new_uniform_cube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L,
    n_points = 500L,
    first_update = 1000L,
    between_update = 1000L,
    verbose = FALSE
  )

  run <- generate(
    sampler,
    max_it = 100L
  )

  expect_equal(nrow(calculate(run)), 600L)
})
