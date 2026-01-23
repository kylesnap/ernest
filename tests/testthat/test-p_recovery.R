#' @srrstats {BS7.0, BS7.1} Parameter recovery for a normal distribution,
#' without any additional information beyond the normal log-likelihood dist.
test_that("Parameter recovery for a normal distribution", {
  prior <- create_normal_prior(mean = c(0, 0))
  log_l <- create_likelihood(
    \(x) {
      LaplacesDemon::dmvn(
        x,
        mu = c(0, 0),
        Sigma = diag(2),
        log = TRUE
      )
    }
  )

  sampler <- ernest_sampler(log_l, prior, nlive = 100, seed = 42)
  run <- generate(sampler, max_iterations = 1000)
  draws <- as_draws(run) |> posterior::resample_draws()
  smry <- posterior::summarise_draws(
    draws,
    \(x) quantile(x, probs = c(0.05, 0.95))
  )

  expect_lte(-1.644854, smry[1, 2])
  expect_gte(1.644854, smry[1, 3])
  expect_lte(-1.644854, smry[2, 2])
  expect_gte(1.644854, smry[2, 3])
})

#' @srrstats {G5.0, G5.6, G5.6a, BS7.2} Use the NIST dataset `mcmc01` to test
#' ernest's ability to recover the parameters of a known distribution. Uses the
#' certified 95% posterior quantile.
#' @srrstats {BS7.4, BS7.4a} Also test to ensure that ernest's output behaves on
#' the same scale as input data wrapped in the likelihood function.
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

test_that("Parameter recovery (NIST mcmc01)", {
  sampler <- ernest_sampler(
    log_lik,
    prior,
    seed = 42
  )
  run <- generate(sampler)
  draws <- as_draws(run) |>
    posterior::resample_draws()

  median_mean <- median(posterior::subset_draws(draws, "mu"))
  expect_gt(median_mean, mean_lower)
  expect_lt(median_mean, mean_upper)

  median_sd <- median(posterior::subset_draws(draws, "sigma"))
  expect_gt(median_sd, sd_lower)
  expect_lt(median_sd, sd_upper)
})
