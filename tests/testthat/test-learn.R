withr::local_seed(42)
data("example_run")


test_that("learn rejects bad parameters", {
  expect_error(
    learn(example_run, times = 0),
    "must be a whole number larger than or equal to 1"
  )
  expect_error(
    learn(example_run, include_weights = "yes"),
    "must be `TRUE` or `FALSE`"
  )
  expect_error(
    learn(example_run, units = "bad"),
    'must be one of "original" or "unit_cube"'
  )
})

test_that("learn returns expected structure", {
  res <- learn(example_run, units = "unit_cube")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 100)
  expect_type(res$log_evidence, "double")
  expect_named(res, c("log_evidence", "x", "y", "z"))
})

test_that("learn is reproducible with a fixed seed", {
  withr::local_seed(123)
  out1 <- learn(example_run, times = 5)

  withr::local_seed(123)
  out2 <- learn(example_run, times = 5)

  expect_identical(out1, out2)
})

test_that("learn returns include_weights", {
  res <- learn(example_run, times = 4, include_weights = TRUE)
  expect_equal(nrow(res), 4)
  expect_type(res$weight, "list")
  expect_all_equal(vapply(res$weight, \(x) sum(x), double(1)), 1)
})
