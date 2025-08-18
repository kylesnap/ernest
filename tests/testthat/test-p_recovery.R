#' Paramter Recovery
#'
#' @srrstats {G5.0, G5.6, G5.6a, BS7.2} Use the NIST dataset `mcmc01` to test
#' ernest's ability to recover the parameters of a normal distribution. Uses the
#' certified 95% posterior quantile.
NULL

# From: https://www.itl.nist.gov/div898/strd/mcmc/mcmc01_cv.html

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
  n_dim = 2L,
  lower = c(99999999, 0.01),
  upper = c(100000001, 1),
  varnames = c("mu", "sigma")
)

# Certified posterior median and 95% interval for mean (from NIST)
mean_median <- 100000000.200000000000000
mean_lower <- 100000000.132819085883166
mean_upper <- 100000000.267180914116834

# Certified posterior median and 95% interval for sd (from NIST)
sd_median <- 0.108372230793914
sd_lower <- 0.069871704416342
sd_upper <- 0.175493354741336

test_that("Parameter recovery (NIST mcmc01)", {
  sampler <- ernest_sampler(
    log_lik,
    prior
  )
  run <- generate(sampler, seed = 42L)
  draws <- as_draws(run) |>
    posterior::resample_draws()

  median_mean <- median(posterior::subset_draws(draws, "mu"))
  expect_gt(median_mean, mean_lower)
  expect_lt(median_mean, mean_upper)

  median_sd <- median(posterior::subset_draws(draws, "sigma"))
  expect_gt(median_sd, sd_lower)
  expect_lt(median_sd, sd_upper)
})

#' @srrstats {G5.6b, G5.9, G5.9a, G5.9b} Tests that parameters are
#' recovered under different seeds and with random noise added to `y`.
test_that("Parameter recovery under a different seed", {
  skip_if(getOption("ernest.extended_tests", FALSE), "Skipping extended tests")
  sampler <- ernest_sampler(
    log_lik,
    prior
  )
  run <- generate(sampler, seed = 24L)
  draws <- as_draws(run) |>
    posterior::resample_draws()

  median_mean <- median(posterior::subset_draws(draws, "mu"))
  expect_gt(median_mean, mean_lower)
  expect_lt(median_mean, mean_upper)

  median_sd <- median(posterior::subset_draws(draws, "sigma"))
  expect_gt(median_sd, sd_lower)
  expect_lt(median_sd, sd_upper)
})

test_that("Parameter recovery with noisy y", {
  skip_if(getOption("ernest.extended_tests", FALSE), "Skipping extended tests")
  jitter_log_lik <- function(theta) {
    if (theta[2] <= 0) {
      return(-Inf)
    }
    sum(dnorm(
      y + rnorm(11, sd = .Machine$double.xmin),
      mean = theta[1],
      sd = theta[2],
      log = TRUE
    ))
  }
  sampler <- ernest_sampler(jitter_log_lik, prior)
  run <- generate(sampler, seed = 24L)
  draws <- as_draws(run) |>
    posterior::resample_draws()

  median_mean <- median(posterior::subset_draws(draws, "mu"))
  expect_gt(median_mean, mean_lower)
  expect_lt(median_mean, mean_upper)

  median_sd <- median(posterior::subset_draws(draws, "sigma"))
  expect_gt(median_sd, sd_lower)
  expect_lt(median_sd, sd_upper)
})
