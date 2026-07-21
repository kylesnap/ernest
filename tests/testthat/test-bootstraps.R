withr::local_seed(42)
data("example_run")

test_that("bootstraps input validation", {
  expect_error(
    bootstraps(example_run, times = -1),
    "whole number larger than or equal to 0."
  )
  expect_error(
    bootstraps(example_run, draws = "bad"),
    'must be one of "none", "original", or "unit_cube"'
  )
})

test_that("bootstraps returns expected structure", {
  res <- bootstraps(example_run, times = 100)

  expect_s3_class(res, "tbl_df")
  expect_named(
    res,
    c(
      "id",
      "split",
      "nlive",
      "nvar",
      "niter",
      "neval",
      "log_evidence",
      "log_evidence_err",
      "information"
    )
  )
  expect_named(
    res$split[[1]],
    c("key", "times")
  )
  expect_all_equal(res$nlive, 1000)
  expect_lte(
    example_run$log_evidence - mean(res$log_evidence),
    sd(res$log_evidence)
  )
})

test_that("bootstraps includes apparent run when apparent = TRUE", {
  res <- bootstraps(example_run, times = 10, apparent = TRUE)
  expect_equal(res$id[11], "Apparent")
  expect_equal(res$log_evidence[[11]], glance(example_run)$log_evidence)
})

test_that("bootstraps returns draws", {
  res <- bootstraps(
    example_run,
    times = 10,
    apparent = TRUE,
    draws = "original"
  )
  expect_s3_class(res$draws[[1]], "draws_matrix")
  expect_equal(
    posterior::variables(res$draws[[1]]),
    posterior::variables(as_draws_matrix(example_run))
  )
  expect_identical(res$draws[[11]], as_draws_matrix(example_run))

  res <- bootstraps(
    example_run,
    times = 10,
    apparent = TRUE,
    draws = "unit_cube"
  )
  expect_identical(
    res$draws[[11]],
    as_draws_matrix(example_run, units = "unit_cube")
  )
})

describe("bootstraps(in_parallel)", {
  it("fails when no daemons are set", {
    expect_error(
      bootstraps(example_run, in_parallel = TRUE),
      "No daemons set."
    )
  })

  it("matches the serial implementation when daemons are available", {
    mirai::daemons(1, dispatcher = FALSE)
    on.exit(mirai::daemons(0), add = TRUE)
    res <- bootstraps(example_run, times = 10, in_parallel = TRUE)
    expect_lte(
      example_run$log_evidence - mean(res$log_evidence),
      sd(res$log_evidence)
    )
  })
})
