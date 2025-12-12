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

#' 2D Gaussian
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
  log_z_analytic = log(2.0 * 2.0 * pi * 0.1 * 0.1 / 100)
)

#' 3D Correlated Multivariate Normal
gaussian_3D <- local({
  n_dim <- 3
  rho <- 0.95
  prior_win <- 10
  mean <- seq(-1, 1, length.out = n_dim)
  cov <- matrix(rho, n_dim, n_dim)
  diag(cov) <- 1
  cov_inv <- solve(cov)
  lnorm <- -0.5 * (log(2 * pi) * n_dim + log(det(cov)))
  log_z_analytic <- n_dim * (-log(2 * prior_win))

  list(
    log_lik = function(x) {
      -0.5 * drop((x - mean) %*% cov_inv %*% (x - mean)) + lnorm
    },
    prior = create_uniform_prior(
      lower = -prior_win,
      upper = prior_win,
      names = LETTERS[1:n_dim]
    ),
    log_z_analytic = log_z_analytic
  )
})

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
  log_z_raster = 235.89516
  # Estimated evidence from a 100 point run with nestle: 236.027 (0.109)
)

#' Correctness Tests
#'
#' @srrstats {G5.4, G5.4b, G5.5} These functions run correctness tests against
#' results found by nestle and through analytic methods for each LRPS.

#' Expect nested sampling results to match expected values.
#'
#' @param ... Arguments passed to `ernest_sampler()`.
#' @param .expected_log_z Numeric. Expected log-evidence value.
#' @param .generate Optional. Arguments for the `generate()` function.
#' @param .seed Integer. Random seed for reproducibility.
#'
#' @return The run object (invisible).
expect_run <- function(..., .expected_log_z, .generate = NULL, .seed = 42L) {
  sampler <- ernest_sampler(..., seed = .seed)
  run <- inject(generate(sampler, !!!.generate))
  log_z <- tail(run$log_evidence, 1)
  log_z_err <- sqrt(tail(run$log_evidence_var, 1))

  delta_log_z <- abs(log_z - .expected_log_z)
  if (delta_log_z > 3.0 * log_z_err) {
    fail(c(
      sprintf(
        "Log-evidence falls outside expectation (%.3f)",
        .expected_log_z
      ),
      sprintf("Run Estimate: %.3f (ERR: %.3f)", log_z, log_z_err)
    ))
  }
  tot_weight <- sum(exp(run$log_weight - log_z))
  if (abs(tot_weight - 1) > sqrt(.Machine$double.eps)) {
    fail(sprintf(
      "Log-weights should sum to one, not %.3f.",
      tot_weight
    ))
  }
  pass()
  invisible(run)
}

#' Runs a sampler on the 2D Gaussian blobs test problem.
#'
#' @param sampler LRPS object to test.
#' @param n_points Integer. Number of live points.
#' @param ... Additional arguments passed to `expect_run()`.
#' @param .generate Optional. Arguments for the `generate()` function.
#' @param .seed Integer. Random seed.
#'
#' @return The run object (invisible).
expect_gaussian_run <- function(
  sampler,
  n_points = 100,
  ...,
  .generate = NULL,
  .seed = 42L
) {
  expect_run(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior,
    sampler = sampler,
    n_points = n_points,
    ...,
    .expected_log_z = gaussian_blobs$log_z_analytic,
    .generate = .generate,
    .seed = .seed
  )
}

#' Runs a sampler on the 3D correlated Gaussian test problem.
#'
#' @param sampler Sampler object to test.
#' @param n_points Integer. Number of live points.
#' @param ... Additional arguments passed to `expect_run()`.
#' @param .generate Optional. Arguments for the `generate()` function.
#' @param .seed Integer. Random seed.
#'
#' @return The run object (invisible).
expect_3D_run <- function(
  sampler,
  n_points = 100,
  ...,
  .generate = NULL,
  .seed = 42L
) {
  expect_run(
    log_lik = gaussian_3D$log_lik,
    prior = gaussian_3D$prior,
    sampler = sampler,
    n_points = n_points,
    ...,
    .expected_log_z = gaussian_3D$log_z_analytic,
    .generate = .generate,
    .seed = .seed
  )
}

#' Runs a sampler on the eggbox test problem
#'
#' @param sampler Sampler object to test.
#' @param n_points Integer. Number of live points.
#' @param ... Additional arguments passed to `expect_run()`.
#' @param .generate Optional. Arguments for the `generate()` function.
#' @param .seed Integer. Random seed.
#'
#' @return The run object (invisible).
expect_eggbox_run <- function(
  sampler,
  n_points = 100,
  ...,
  .generate = NULL,
  .seed = 42L
) {
  expect_run(
    log_lik = eggbox$log_lik,
    prior = eggbox$prior,
    sampler = sampler,
    n_points = 100,
    ...,
    .expected_log_z = eggbox$log_z_raster,
    .generate = .generate,
    .seed = .seed
  )
}
