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
  res <- learn(example_run)

  expect_s3_class(res, c("ernest_resample", "tbl_df"))
  expect_equal(nrow(res), 100)
  expect_type(res$log_evidence, "double")
  expect_named(res, c("log_evidence", "x", "y", "z"))
  expect_snapshot(res)
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
  expect_type(res$weights, "list")
  expect_all_equal(vapply(res$weights, \(x) sum(x$weight), double(1)), 1)
})

#' @srrstats {G5.0, G5.6, G5.6a, BS7.2} Use the NIST dataset `mcmc01` to test
#' ernest's ability to recover the parameters of a known distribution. Uses the
#' certified 95% posterior quantile.
#' @srrstats {BS7.4, BS7.4a} Also test to ensure that ernest's output behaves on
#' the same scale as input data wrapped in the likelihood function.
#' From: https://www.itl.nist.gov/div898/strd/mcmc/mcmc01_cv.html
NULL

describe("Parameter recovery", {
  # NIST mcmc01: likelihood and prior
  y <- c(
    100000000.2,
    100000000.1,
    100000000.3,
    100000000.1,
    100000000.3,
    100000000.1,
    100000000.3,
    100000000.1,
    100000000.3,
    100000000.1,
    100000000.3
  )

  log_lik <- function(theta) {
    if (theta[2] <= 0) {
      return(-Inf)
    }
    sum(dnorm(y, mean = theta[1], sd = theta[2], log = TRUE))
  }

  prior <- create_uniform_prior(
    lower = c(99999999, 0.01),
    upper = c(100000001, 1),
    names = c("mu", "sigma")
  )

  # Certified posterior median and 95% interval for mean (from NIST)
  mean_median <- 100000000.200000000000000
  mean_lower <- 100000000.132819085883166
  mean_upper <- 100000000.267180914116834

  # Certified posterior median and 95% interval for sd (from NIST)
  sd_median <- 0.108372230793914
  sd_lower <- 0.069871704416342
  sd_upper <- 0.175493354741336

  sampler <- ernest_sampler(
    log_lik,
    prior,
    seed = 42
  )
  run <- generate(sampler)

  it("Recovers parameters in the ernest_run object", {
    draws <- as_draws(run) |>
      posterior::resample_draws()

    median_mean <- median(posterior::subset_draws(draws, "mu"))
    expect_gt(median_mean, mean_lower)
    expect_lt(median_mean, mean_upper)

    median_sd <- median(posterior::subset_draws(draws, "sigma"))
    expect_gt(median_sd, sd_lower)
    expect_lt(median_sd, sd_upper)
  })

  it("Recovers parameters in the ernest_resample object", {
    bs <- learn(run)

    median_mean <- median(bs$mu)
    expect_gt(median_mean, mean_lower)
    expect_lt(median_mean, mean_upper)

    median_sd <- median(bs$sigma)
    expect_gt(median_sd, sd_lower)
    expect_lt(median_sd, sd_upper)
  })
})
