test_that("Tidy output", {
  lrps <- new_rwmh_cube(
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2,
    update_interval = 10,
    num_steps = 20,
    target_acceptance = 0.25,
    epsilon = 0.1
  )
  sampler <- ernest_sampler$new(lrps, c("A", "B"), 100, FALSE)
  sampler <- compile(sampler)

  result <- sampler$generate(max_iterations = 100L)
  expect_equal(calculate(result) |> nrow(), 200L)
})
