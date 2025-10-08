fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

test_that("unif_ellipsoid returns correct class and structure", {
  obj <- unif_ellipsoid(1.5)
  expect_s3_class(obj, c("unif_ellipsoid", "ernest_lrps"), exact = TRUE)
  expect_null(obj$unit_log_fn)
  expect_null(obj$n_dim)
  expect_equal(obj$max_loop, 1e6L)
  expect_equal(obj$enlarge, 1.5)
  expect_null(obj$cache$cov)
  expect_null(obj$cache$d2)
  expect_null(obj$cache$loc)

  obj <- unif_ellipsoid()
  expect_equal(obj$enlarge, 1.25)
})

describe("new_unif_ellipsoid", {
  it("fails when scale or n_dim are improper.", {
    expect_snapshot(new_unif_ellipsoid(fn, 2L, enlarge = 0.5), error = TRUE)
  })

  it("passes the correct defaults", {
    obj <- new_unif_ellipsoid(fn, n_dim = 2)
    expect_s3_class(obj, c("unif_ellipsoid", "ernest_lrps"), exact = TRUE)
    expect_equal(obj$unit_log_fn, fn)
    expect_equal(obj$n_dim, 2L)
    expect_equal(obj$max_loop, 1e6L)
    expect_equal(obj$enlarge, 1.0)
    expect_equal(obj$cache$cov, diag(2))
    expect_equal(obj$cache$scale, 0.5)
    expect_equal(obj$cache$loc, c(0.5, 0.5))
    expect_equal(obj$cache$log_volume, log(pi * (sqrt(2) / 2)^2))
    expect_snapshot(obj)
  })
})

uniform <- new_unif_ellipsoid(fn, n_dim = 2, enlarge = 1.2)
describe("propose.unif_ellipsoid", {
  it("Proposes points in the unit cube", {
    result <- propose(uniform, original = NULL, criterion = -Inf)
    expect_length(result$unit, 2)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_equal(uniform$cache$n_call, 0L)
  })

  it("Proposes points in the unit sphere", {
    result <- propose(uniform, original = c(0.5, 0.5), criterion = -99.3068)
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
  propose(uniform, original = c(0.5, 0.5), criterion = -99.3068)
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
    ell <<- list(
      cov = new_uniform$cache$cov,
      loc = new_uniform$cache$loc,
      scale = new_uniform$cache$scale,
      trans = new_uniform$cache$trans
    )
    new_live <- replicate(
      500,
      propose(new_uniform, original = c(0.5, 0.5), criterion = -Inf)
    )
    new_live <- do.call(rbind, new_live["unit", ])
    precision <- solve(ell$cov)
    dists <- apply(new_live, 1, \(x) {
      d <- x - ell$loc
      drop(d %*% precision %*% d)
    })
    n_oob <- sum(dists > ell$scale * 1.2)
    expect_equal(n_oob, 0)
    expect_snapshot(uniform)
  })

  it("scales to the correct log_volume", {
    unif_scaled <- new_unif_ellipsoid(fn, n_dim = 2, enlarge = 1.25)
    new_unif_scaled <- update_lrps(uniform, live)
    scaled_live <- replicate(
      500,
      propose(new_unif_scaled, original = c(0.5, 0.5), criterion = -Inf)
    )
    scaled_live <- do.call(rbind, scaled_live["unit", ])
    precision <- solve(ell$cov)
    dists <- apply(scaled_live, 1, \(x) {
      d <- x - ell$loc
      drop(d %*% precision %*% d)
    })
    n_oob <- sum(dists > ell$scale * 1.25)
    expect_equal(n_oob, 0)
  })

  it("reports numerical errors", {
    x <- seq(0.01, 0.49, length.out = 500)
    xy <- cbind(x, x * 2)
    expect_snapshot(new_uniform <- update_lrps(uniform, xy))
  })
})
