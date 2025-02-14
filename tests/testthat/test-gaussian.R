test_that("2D Gaussian Likelihood (Uniform Cube)", {
  gauss <- make_gaussian(2)
  sampler <- UniformCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    num_dim = 2L
  )

  run_1 <- nested_sampling(sampler)
  run_1 |> print()

  generate(run_1, max_it = 100, max_call = 100000, dlogz = 0.5) |>
    print()
  calculate(run_1) |>
    print()
  glance(run_1) |>
    print()

  sampler <- RandomWalkCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    num_dim = 2L
  )
  run_2 <- nested_sampling(sampler)

  generate(run_2, max_it = 10000, dlogz = 0.5) |>
    print()
  glance(run_2) |> print()
})
