lrps <- new_rwmh_cube(
  log_lik = gaussian_2$log_lik,
  prior_transform = gaussian_2$prior_transform,
  n_dim = 2,
  update_interval = 200,
  num_steps = 20,
  target_acceptance = 0.25,
  epsilon = 0.1
)

test_that("Ernest_Lrps Formating Works", {
  expect_snapshot(lrps)
})

test_that("Ernest_Sampler Formatting Works", {
  sampler <- ernest_sampler$new(
    lrps = lrps,
    ptype = c("A", "B"),
    n_points = 100L,
    verbose = FALSE
  )
  expect_snapshot(sampler)

  set.seed(667)
  compile(sampler)
  run <- generate(
    sampler,
    max_iterations = 100L
  )
  expect_snapshot(run)
})
