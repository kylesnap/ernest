withr::local_seed(42)
data("example_run")

test_that("generate_bootstraps input validation", {
  expect_error(generate_bootstraps(example_run, times = -1))
  expect_error(generate_bootstraps(example_run, times = 1.5))
  expect_error(
    generate_bootstraps(example_run, units = "bad"),
    'must be one of "original" or "unit_cube"'
  )
})

test_that("generate_bootstraps returns expected structure", {
  res <- generate_bootstraps(example_run, times = 100)

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 100)
  expect_named(res, c("id", "split", "run"))
  expect_type(res$id, "integer")
  expect_type(res$split, "list")
  expect_type(res$run, "list")
})

test_that("generate_bootstraps is reproducible with a fixed seed", {
  withr::local_seed(123)
  out1 <- generate_bootstraps(example_run, times = 5)

  withr::local_seed(123)
  out2 <- generate_bootstraps(example_run, times = 5)

  expect_identical(out1, out2)
})

test_that("generate_bootstraps handles times = 0", {
  res0 <- generate_bootstraps(example_run, times = 0)
  expect_equal(nrow(res0), 0)
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

  it("Recovers parameters via bootstrapping", {
    bs <- generate_bootstraps(run, times = 100)
    means <- vapply(
      bs$run,
      \(run) {
        m <- posterior::summarise_draws(run, "mean")$mean
        names(m) <- c("mu", "sigma")
        m
      },
      numeric(2)
    )
    median_mean <- median(means["mu", ])
    expect_gt(median_mean, mean_lower)
    expect_lt(median_mean, mean_upper)
    median_sd <- median(means["sigma", ])
    expect_gt(median_sd, sd_lower)
    expect_lt(median_sd, sd_upper)
  })
})
