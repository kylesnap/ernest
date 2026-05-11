test_that("Calculate weights from runs and rcrds", {
  expect_equal(
    weights(example_run, log = TRUE),
    example_run$log_weight - example_run$log_evidence
  )
  expect_equal(sum(weights(example_run)), 1)
  expect_equal(sum(weights(example_run$rcrd)), 1)
})

test_that("weights fails when the rcrd is unsorted", {
  x <- sample(example_run$rcrd, size = length(example_run$rcrd))
  expect_error(weights(x), "Can't estimate weights.")
})
