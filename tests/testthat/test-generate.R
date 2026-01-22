skip("TEMPORARY")
data(example_run)

test_that("generate fails with poor arguments", {
  max_iterations <- example_run$n_iter
  expect_error(
    generate(example_run, max_iterations = example_run$n_iter),
    "`max_iterations` must be strictly larger"
  )

  expect_error(
    generate(example_run, max_evaluations = example_run$neval),
    "`max_evaluations` must be strictly larger"
  )

  expect_error(
    generate(example_run, min_logz = 0.05),
    "`min_logz` must be strictly smaller"
  )

  new_sampler <- compile(example_run, clear = TRUE)
  expect_error(
    generate(new_sampler, min_logz = 0),
    "Can't perform nested sampling without any stopping criteria."
  )
})

test_that("generate can continue a previous run", {
  continued <- generate(example_run, min_logz = 0.025)
  prev_dead <- example_run$n_iter - example_run$n_points
  expect_identical(
    example_run$samples[1:prev_dead, ],
    continued$samples[1:prev_dead, ]
  )
})
