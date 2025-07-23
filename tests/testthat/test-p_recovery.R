#' Paramter Recovery
#'
#' @srrstats {G5.0, G5.6, G5.6a, BS7.2} Use the NIST dataset `mcmc01` to test
#' ernest's ability to recover the parameters of a normal distribution. Uses the
#' certified 95% posterior quantile.
NULL

# From: https://www.itl.nist.gov/div898/strd/mcmc/mcmc01_cv.html
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
  sum(dnorm(y, theta[1], theta[2], log = TRUE))
}

prior <- create_normal_prior(
  n_dim = 2,
  mean = c(100000000, 0.1),
  sd = 10,
  lower = c(-Inf, 0),
  varnames = c("mean", "sd")
)

test_that("Parameter recovery", {
  set.seed(42L)

  sampler <- nested_sampling(log_lik, prior)
  run <- generate(sampler)
  draws <- as_draws(run) |>
    posterior::resample_draws()

  median_mean <- median(posterior::subset_draws(draws, "mean"))
  expect_lt(
    100000000.132819085883166,
    median_mean
  )
  expect_lt(
    median_mean,
    100000000.267180914116834
  )

  median_sd <- median(posterior::subset_draws(draws, "sd"))
  expect_lt(
    0.069871704416342,
    median_sd
  )
  expect_lt(
    median_sd,
    0.175493354741336
  )
})

#' @srrstats {G5.6b, G5.9, G5.9a, G5.9b} Tests that parameters are
#' recovered under different seeds and with random noise added to `y`.
test_that("Parameter recovery under a different seed", {
  set.seed(24L)
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()

  sampler <- nested_sampling(log_lik, prior)
  run <- generate(sampler)
  draws <- as_draws(run) |>
    posterior::resample_draws()

  median_mean <- median(posterior::subset_draws(draws, "mean"))
  expect_lt(
    100000000.132819085883166,
    median_mean
  )
  expect_lt(
    median_mean,
    100000000.267180914116834
  )

  median_sd <- median(posterior::subset_draws(draws, "sd"))
  expect_lt(
    0.069871704416342,
    median_sd
  )
  expect_lt(
    median_sd,
    0.175493354741336
  )
})

test_that("Parameter recovery with noisy y", {
  set.seed(42L)
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()
  noisy_ll <- \(theta) {
    y <- y + rnorm(11, mean = 0, sd = .Machine$double.eps)
    sum(dnorm(y, theta[1], theta[2], log = TRUE))
  }
  sampler <- nested_sampling(log_lik, prior)
  run <- generate(sampler)
  draws <- as_draws(run) |>
    posterior::resample_draws()

  median_mean <- median(posterior::subset_draws(draws, "mean"))
  expect_lt(
    100000000.132819085883166,
    median_mean
  )
  expect_lt(
    median_mean,
    100000000.267180914116834
  )

  median_sd <- median(posterior::subset_draws(draws, "sd"))
  expect_lt(
    0.069871704416342,
    median_sd
  )
  expect_lt(
    median_sd,
    0.175493354741336
  )
})
