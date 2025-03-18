gauss <- make_gaussian(3L)
sampler <- nested_sampling(
  gauss$log_lik,
  prior_transform = gauss$prior_transform,
  ptype = 3L
)

test_that("variable calls work", {
  expect_equal(
    variables(sampler),
    c("X...1", "X...2", "X...3")
  )

  expect_error(
    variables(sampler) <- c("A", "B"),
    "must match the original number of dimensions"
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
  sampler$compile()
  compile(
    sampler,
    max_iterations = 100L
  )
  generate(sampler, max_iterations = 100L)

  draws <- as_draws(sampler)
  expect_equal(posterior::niterations(draws), 600L)
  expect_equal(posterior::nvariables(draws), 3L)
  expect_snapshot(draws)

  draws_units <- as_draws_matrix(sampler, scale = "unit")
  expect_true(all(draws_units[,c("X","Y","Z")] >= 0))
  expect_true(all(draws_units[,c("X","Y","Z")] <= 1))

  draws_no_live <- as_draws(sampler, inc_live = FALSE)
  expect_equal(posterior::niterations(draws_no_live), 100L)
})
