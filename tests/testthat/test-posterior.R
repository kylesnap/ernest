gauss <- make_gaussian(3L)
sampler <- nested_sampling(
  gauss$log_lik,
  prior_transform = gauss$prior_transform,
  n_dim = 3L
)

test_that("variable calls work", {
  expect_equal(
    variables(sampler),
    c("X...1", "X...2", "X...3")
  )

  expect_error(
    variables(sampler) <- c("A", "B"),
    "`variables` is a different length"
  )

  variables(sampler) <- c("X", "Y", "Z")
  expect_equal(
    variables(sampler),
    c("X", "Y", "Z")
  )
})

test_that("nvariables works", {
  expect_equal(
    nvariables(sampler),
    3
  )
})

test_that("as_draws works", {
  expect_error(
    as_draws(sampler),
    "No iterations have been run with this sampler."
  )
  expect_error(
    as_draws(sampler, scale = "bloop")
  )

  set.seed(667)
  run <- generate(
    sampler,
    max_it = 100L
  )

  expect_snapshot(posterior::as_draws(run))

  expect_equal(posterior::niterations(posterior::as_draws(run)), 600)
  expect_equal(posterior::niterations(posterior::as_draws(run, scale = "unit")), 600)

  expect_equal(posterior::niterations(posterior::as_draws(run, inc_live = FALSE)), 100)
  expect_equal(posterior::niterations(posterior::as_draws(run, scale = "unit", inc_live = FALSE)), 100)
})
