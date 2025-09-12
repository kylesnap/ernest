fn <- purrr::compose(gaussian_blobs$log_lik, gaussian_blobs$prior$fn)
set.seed(42)

test_that("unif_ellipsoid returns correct class and structure", {
  obj <- unif_ellipsoid(1.5)
  expect_s3_class(obj, c("unif_ellipsoid", "ernest_lrps"), exact = TRUE)
  expect_null(obj$unit_log_fn)
  expect_null(obj$n_dim)
  expect_equal(obj$max_loop, 1e6L)
  expect_equal(obj$enlarge, 1.5)
  expect_null(obj$cache$chol_precision)
  expect_null(obj$cache$d2)
  expect_null(obj$cache$loc)

  expect_snapshot(obj <- unif_ellipsoid())
  expect_equal(obj$enlarge, 1.0)
})

describe("new_unif_ellipsoid", {
  it("fails when scale or n_dim are improper.", {
    expect_snapshot(new_unif_ellipsoid(fn, 2L, enlarge = 0.5), error = TRUE)
    expect_snapshot(new_unif_ellipsoid(fn, 1L), error = TRUE)
  })

  it("passes the correct defaults", {
    obj <- new_unif_ellipsoid(fn, n_dim = 2)
    expect_s3_class(obj, c("unif_ellipsoid", "ernest_lrps"), exact = TRUE)
    expect_equal(obj$unit_log_fn, fn)
    expect_equal(obj$n_dim, 2L)
    expect_equal(obj$max_loop, 1e6L)
    expect_equal(obj$enlarge, 1.0)
    expect_equal(obj$cache$chol_precision, diag(2))
    expect_equal(obj$cache$d2, 2 / 4)
    expect_equal(obj$cache$loc, c(0.5, 0.5))
    expect_equal(obj$cache$volume, pi * (sqrt(2) / 2)^2)
    expect_snapshot(obj)
  })
})

uniform <- new_unif_ellipsoid(fn, n_dim = 2, enlarge = 1.2)
describe("propose.unif_ellipsoid", {
  it("Proposes points in the unit cube", {
    result <- propose(uniform, original = NULL, criteria = -Inf)
    expect_length(result$unit, 2)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_equal(uniform$cache$n_call, 0L)
  })

  it("Proposes points in the unit sphere", {
    result <- propose(uniform, original = c(0.5, 0.5), criteria = -99.3068)
    expect_length(result$unit, 2)
    expect_length(result$unit, 2)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_gt(uniform$cache$n_call, 0L)
    expect_snapshot(uniform)
  })
})

live <- replicate(
  500,
  propose(uniform, original = c(0.5, 0.5), criteria = -99.3068)
)
live <- do.call(rbind, live["unit", ])
describe("update_lrps.unif_ellipsoid", {
  it("is idempotent", {
    new_uniform <- update_lrps(uniform)
    expect_identical(new_uniform, uniform)
  })

  ell <<- NULL
  it("can rebound to a matrix of live points", {
    new_uniform <- update_lrps(uniform, live)
    ell <<- cluster::ellipsoidhull(live)
    precision <- solve(ell$cov)
    loc <- ell$loc
    expect_equal(new_uniform$cache$chol_precision, chol(precision))
    expect_equal(new_uniform$cache$d2, ell$d2 * 1.2)
    expect_equal(new_uniform$cache$loc, loc)
    expect_equal(new_uniform$cache$volume, cluster::volume(ell))

    new_live <- replicate(
      500,
      propose(new_uniform, original = c(0.5, 0.5), criteria = -Inf)
    )
    new_live <- do.call(rbind, new_live["unit", ])
    dists <- apply(new_live, 1, \(x) {
      d <- x - ell$loc
      drop(d %*% precision %*% d)
    })
    n_oob <- sum(dists > ell$d2 * 1.2)
    expect_equal(n_oob, 0)
    expect_snapshot(uniform)
  })

  it("scales to the correct volume", {
    unif_scaled <- new_unif_ellipsoid(fn, n_dim = 2, enlarge = 1.25)
    new_unif_scaled <- update_lrps(uniform, live)
    scaled_live <- replicate(
      500,
      propose(new_unif_scaled, original = c(0.5, 0.5), criteria = -Inf)
    )
    scaled_live <- do.call(rbind, scaled_live["unit", ])
    precision <- solve(ell$cov)
    dists <- apply(scaled_live, 1, \(x) {
      d <- x - ell$loc
      drop(d %*% precision %*% d)
    })
    n_oob <- sum(dists > ell$d2 * 1.25)
    expect_equal(n_oob, 0)
  })

  it("fails to a unit sphere when cluster fails", {
    x <- seq(0.01, 0.49, length.out = 500)
    xy <- cbind(x, x * 2) # Dependence between columns.
    expect_snapshot(new_uniform <- update_lrps(uniform, xy))
    expect_equal(new_uniform$cache$chol_precision, diag(2))
    expect_equal(new_uniform$cache$d2, 2 / 4)
    expect_equal(new_uniform$cache$loc, c(0.5, 0.5))
  })
})

describe("unif_ellipsoid in 3D", {
  log_lik <- function(x) {
    sigma <- 0.1
    mu1 <- c(1, 1, 1)
    mu2 <- -c(1, 1, 1)
    sigma_inv <- diag(3) / 0.1**2

    dx1 <- x - mu1
    dx2 <- x - mu2
    val1 <- -0.5 * drop(dx1 %*% sigma_inv %*% dx1)
    val2 <- -0.5 * drop(dx2 %*% sigma_inv %*% dx2)
    matrixStats::logSumExp(c(val1, val2))
  }
  prior <- create_uniform_prior(lower = -5, upper = 5, varnames = LETTERS[1:3])
  fn <- purrr::compose(log_lik, prior$fn)
  uniform_3d <- new_unif_ellipsoid(fn, n_dim = 3)

  it("can propose in 3D", {
    result <- replicate(
      500,
      propose(uniform_3d, original = c(0.5, 0.5, 0.5), criteria = -300)
    )
    live <<- do.call(rbind, result["unit", ])
    expect_equal(dim(live), c(500, 3))
  })

  it("can update its bounds", {
    new_uniform <- update_lrps(uniform_3d, live)
    ell <- cluster::ellipsoidhull(live)
    precision <- solve(ell$cov)
    new_live <- replicate(
      500,
      propose(new_uniform, original = c(0.5, 0.5, 0.5), criteria = -Inf)
    )
    new_live <- do.call(rbind, new_live["unit", ])
    dists <- apply(new_live, 1, \(x) {
      d <- x - ell$loc
      drop(d %*% precision %*% d)
    })
    n_oob <- sum(dists > ell$d2)
    expect_equal(n_oob, 0)
  })
})
