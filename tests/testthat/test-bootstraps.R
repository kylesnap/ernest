withr::local_seed(42)
data("example_run")

test_that("bootstraps input validation", {
  expect_error(
    bootstraps(example_run, times = -1),
    "whole number larger than or equal to 0."
  )
  expect_error(
    bootstraps(example_run, units = "bad"),
    'must be one of "original" or "unit_cube"'
  )
})

test_that("bootstraps returns expected structure", {
  res <- bootstraps(example_run, times = 100)

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 100)
  expect_named(res, c("id", "split", "run"))
  expect_type(res$id, "character")
  expect_type(res$split, "list")
  expect_type(res$run, "list")

  log_z_est <- vapply(
    res$run,
    \(x) {
      compute_integral(x, truncate = TRUE)[["log_evidence"]]
    },
    numeric(1)
  )
  expect_lte(
    example_run$log_evidence - mean(log_z_est),
    sd(log_z_est)
  )

  n_tot <- vapply(
    res$split,
    \(x) sum(x$times),
    integer(1)
  )
  expect_all_equal(n_tot, example_run$nlive)
})

test_that("bootstraps is reproducible with a fixed seed", {
  withr::with_preserve_seed(out1 <- bootstraps(example_run, times = 5))
  withr::with_preserve_seed(out2 <- bootstraps(example_run, times = 5))
  expect_identical(out1, out2)
})

test_that("bootstraps handles times = 0", {
  res0 <- bootstraps(example_run, times = 0)
  expect_equal(nrow(res0), 0)
})

test_that("bootstraps includes apparent run when apparent = TRUE", {
  res <- bootstraps(example_run, times = 10, apparent = TRUE)

  expect_equal(nrow(res), 11)
  expect_equal(res$id[11], "Apparent")
  expect_equal(res$split[[11]], seq(example_run$nlive))
  expect_identical(res$run[[11]], example_run$rcrd)
})
