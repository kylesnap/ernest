#' NESTLE TEST CASES
#'
#' This file contains problems used by the python package `nestle` to
#' check their implementation of the NS algorithm. We translated them into
#' R, cross-checked their performance relative to the original python,
#' and now use them to test ernest.
#'
#' @srrstats {BS4.1} Test cases developed by `nestle` are used to internally
#' test ernest's ability to conduct nested sampling. These are also demonstrated
#' in vignettes.
#'
#' @source https://github.com/kbarbary/nestle/blob/master/runtests.py
NULL

#' Two gaussians centered at (1, 1) and (-1, -1) with sigma = 0.1
gaussian_blobs <- list(
  log_lik = function(x) {
    sigma <- 0.1
    mu1 <- c(1, 1)
    mu2 <- -c(1, 1)
    sigma_inv <- diag(2) / 0.1**2

    dx1 <- x - mu1
    dx2 <- x - mu2
    val1 <- -0.5 * drop(dx1 %*% sigma_inv %*% dx1)
    val2 <- -0.5 * drop(dx2 %*% sigma_inv %*% dx2)
    matrixStats::logSumExp(c(val1, val2))
  },
  prior = create_uniform_prior(lower = -5, upper = 5, names = LETTERS[1:2]),
  # Analytic evidence for two Gaussian blobs
  analytic_z = log(2.0 * 2.0 * pi * 0.1 * 0.1 / 100),
  # Raster-calculated evidence from nestle's test suite
  raster_z = -6.679316,
  # Estimated evidence from a 100 point run with nestle
  estimated_z = -6.778,
  estimated_z_err = 0.238
)

# Eggbox Distribution
eggbox <- list(
  log_lik = function(x) {
    tmax <- 5.0 * pi
    vec_flag <- !is.matrix(x)
    if (vec_flag) {
      dim(x) <- c(1, length(x))
    }

    t <- sweep(2.0 * tmax * x, 2, tmax, "-")
    val <- (2.0 + cos(t[, 1] / 2.0) * cos(t[, 2] / 2.0))^5.0
    if (vec_flag) {
      val[[1]]
    } else {
      val
    }
  },
  # Standard uniform prior
  prior = create_uniform_prior(names = LETTERS[1:2]),
  # Raster-calculated evidence from nestle's test suite
  raster_z = 235.89516,
  # Estimated evidence from a 100 point run with nestle
  estimated_z = 236.027,
  estimated_z_err = 0.109
)

#' 3D Correlated Multivariate Normal (dynesty test)
mvm_3d <- local({
  n_dim <- 3
  rho <- 0.95
  prior_win <- 10
  mean <- seq(-1, 1, length.out = n_dim)
  cov <- matrix(rho, n_dim, n_dim)
  diag(cov) <- 1
  cov_inv <- solve(cov)
  lnorm <- -0.5 * (log(2 * pi) * n_dim + log(det(cov)))
  analytic_z <- n_dim * (-log(2 * prior_win))

  list(
    log_lik = function(x) {
      -0.5 * drop((x - mean) %*% cov_inv %*% (x - mean)) + lnorm
    },
    prior = create_uniform_prior(
      lower = -prior_win,
      upper = prior_win,
      names = LETTERS[1:n_dim]
    ),
    analytic_z = analytic_z
  )
})


#' Correctness Tests
#'
#' @srrstats {G5.4, G5.4b, G5.5} This function runs a correctness tests against
#' results found by nestle and through analytic methods.

run_gaussian_blobs <- function(sampler, ..., tolerance = 1) {
  sampler <- exec(
    ernest_sampler,
    gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior,
    sampler = sampler,
    !!!list2(...),
    seed = 42
  )
  result <- generate(sampler)
  smry <- summary(result)

  expect_lt(
    abs(smry$log_evidence - gaussian_blobs$analytic_z),
    log(tolerance) + smry$log_evidence_err
  )
  expect_snapshot(result)

  weights <- as_draws(result) |>
    weights()
  expect_equal(sum(weights), 1)

  # Test seed and rerunning
  withr::local_seed(45)
  cur_seed <- .Random.seed[1]
  res1 <- generate(sampler, max_iterations = 1000)
  expect_equal(cur_seed, .Random.seed[1])
  res2 <- generate(res1, max_iterations = 2000)
  expect_equal(cur_seed, .Random.seed[1])
  res2_cpy <- generate(res1, max_iterations = 2000)
  expect_equal(cur_seed, .Random.seed[1])
  expect_identical(res2$log_lik, res2_cpy$log_lik)
}

run_eggbox <- function(sampler, ..., tolerance = 1) {
  sampler <- exec(
    ernest_sampler,
    eggbox$log_lik,
    eggbox$prior,
    sampler = sampler,
    !!!list2(...),
    seed = 42
  )
  result <- generate(sampler)
  smry <- summary(result)

  expect_lt(
    abs(smry$log_evidence - eggbox$raster_z),
    log(tolerance) + smry$log_evidence_err
  )
  expect_snapshot(result)

  weights <- as_draws(result) |>
    weights()
  expect_equal(sum(weights), 1)
}

run_3d <- function(sampler, ..., tolerance = 1) {
  sampler <- exec(
    ernest_sampler,
    mvm_3d$log_lik,
    mvm_3d$prior,
    sampler = sampler,
    !!!list2(...),
    seed = 42
  )
  result <- generate(sampler)
  smry <- summary(result)

  expect_lt(
    abs(smry$log_evidence - mvm_3d$analytic_z),
    log(tolerance) + smry$log_evidence_err
  )
  expect_snapshot(result)

  weights <- as_draws(result) |>
    weights()
  expect_equal(sum(weights), 1)
}
