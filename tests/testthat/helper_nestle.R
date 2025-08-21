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
    matrixStats::logSumExp(c(
      -drop(dx1 %*% (sigma_inv %*% dx1)) / 2.0,
      -drop(dx2 %*% (sigma_inv %*% dx2)) / 2.0
    ))
  },
  prior = create_uniform_prior(2L, lower = -5, upper = 5),
  # Analytic evidence for two Gaussian blobs
  analytic_z = log(2.0 * 2.0 * pi * 0.1 * 0.1 / 100),
  # Raster-calculated evidence from nestle's test suite
  raster_z = -6.679316,
  # Estimated evidence from a 100 point run with nestle
  estimated_z = -6.778,
  estimated_z_err = 0.238
)
