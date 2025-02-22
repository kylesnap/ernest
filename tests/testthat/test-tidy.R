test_that("Tidy output", {
  gauss <- make_gaussian(3L)
  sampler <- UniformCube(
    log_lik = gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 3L
  )

  run <- generate(
    sampler,
    max_it = 100L
  )

  expect_equal(
    nrow(tidy(run)),
    nrow(tidy(run, exponentiate = FALSE))
  )

})
