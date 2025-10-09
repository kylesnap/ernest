fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

describe("BoundingEllipsoid", {
  it("fits points in 3D correctly", {
    A <- matrix(
      c(1.439, -1.607, 0.626, -1.607, 2.685, -0.631, 0.626, -0.631, 0.43),
      nrow = 3,
      byrow = TRUE
    )
    n_points <- 5000
    radius <- 2
    original_points <- uniformly::runif_in_ellipsoid(n_points, A, radius)
    theoretical_cov <- (radius^2 / (3 + 2)) * solve(A)

    ell_fit <- BoundingEllipsoid(original_points)
    expect_false(is.infinite(ell_fit$log_vol))
    expect_equal(ell_fit$error, 0)

    new_points <- uniformly::runif_in_sphere(n_points, 3, 1) %*%
      ell_fit$scaledInvSqrtA
    new_points <- sweep(new_points, 2, ell_fit$loc, "+")

    expect_equal(colMeans(new_points), c(0, 0, 0), tolerance = 0.05)
    expect_equal(ell_fit$loc, colMeans(original_points), tolerance = 0.1)

    sample_cov <- cov(original_points)
    fitted_cov <- cov(new_points)
    expect_equal(fitted_cov, theoretical_cov, tolerance = 0.05)
    expect_equal(fitted_cov, sample_cov, tolerance = 0.05)
  })

  it("fits points in 5D correctly", {
    A <- matrix(nrow = 5, ncol = 5, byrow = TRUE)
    A[1, ] <- c(0.228, 0.0948, -0.133, -0.174, 0.00331)
    A[2, ] <- c(0.0948, 0.174, -0.0954, -0.146, 0.00501)
    A[3, ] <- c(-0.133, -0.0954, 0.268, -0.0323, -0.00409)
    A[4, ] <- c(-0.174, -0.146, -0.0323, 0.386, -0.00151)
    A[5, ] <- c(0.00331, 0.00501, -0.00409, -0.00151, 0.0678)
    A <- 1e4 * A
    n_points <- 5000
    radius <- 1
    original_points <- uniformly::runif_in_ellipsoid(n_points, A, 1)

    theoretical_cov <- (radius^2 / (3 + 2)) * solve(A)

    ell_fit <- BoundingEllipsoid(original_points)
    expect_false(is.infinite(ell_fit$log_vol))
    expect_equal(ell_fit$error, 0)

    new_points <- uniformly::runif_in_sphere(n_points, 5, 1) %*%
      ell_fit$scaledInvSqrtA
    expect_equal(colMeans(new_points), c(0, 0, 0, 0, 0), tolerance = 0.05)
    expect_equal(ell_fit$loc, colMeans(original_points), tolerance = 0.1)

    sample_cov <- cov(original_points)
    fitted_cov <- cov(new_points)
    expect_equal(fitted_cov, theoretical_cov, tolerance = 0.05)
    expect_equal(fitted_cov, sample_cov, tolerance = 0.1)
  })
})

test_that("unif_ellipsoid returns correct class and structure", {
  obj <- unif_ellipsoid(1.5)
  expect_s3_class(obj, c("unif_ellipsoid", "ernest_lrps"), exact = TRUE)
  expect_null(obj$unit_log_fn)
  expect_null(obj$n_dim)
  expect_equal(obj$max_loop, 1e6L)
  expect_equal(obj$enlarge, 1.5)
  expect_mapequal(as.list(obj$cache), list(n_call = 0L))

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
    expect_mapequal(
      env_get_list(
        obj$cache,
        c("A", "loc", "scaledInvSqrtA", "scale", "log_volume")
      ),
      list(
        A = diag(2),
        loc = c(0.5, 0.5),
        scaledInvSqrtA = diag(2) * 0.5,
        scale = 0.5,
        log_volume = log(pi * (sqrt(2) / 2)^2)
      )
    )
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
      A = new_uniform$cache$A,
      loc = new_uniform$cache$loc,
      scale = new_uniform$cache$scale
    )
    new_live <- replicate(
      500,
      propose(new_uniform, original = c(0.5, 0.5), criterion = -Inf)
    )
    new_live <- do.call(rbind, new_live["unit", ])

    precision <- ell$A
    dists <- apply(new_live, 1, \(x) {
      d <- x - ell$loc
      drop(d %*% precision %*% d)
    })
    n_oob <- sum(dists > ell$scale * 1.2)
    expect_equal(n_oob, 0)
    expect_snapshot(uniform)
  })

  it("reports numerical errors", {
    x <- seq(0.01, 0.49, length.out = 500)
    xy <- cbind(x, x * 2)
    expect_snapshot(new_uniform <- update_lrps(uniform, xy))
  })
})
