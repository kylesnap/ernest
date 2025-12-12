set.seed(42)
fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()

test_that("unif_ellipsoid can be called by user", {
  default <- unif_ellipsoid()
  expect_snapshot(unif_ellipsoid(enlarge = 0.5), error = TRUE)
  expect_snapshot(unif_ellipsoid(enlarge = 1))
  expect_equal(default$enlarge, 1.25)
  expect_snapshot(default)
})

describe("unif_ellipsoid class", {
  ptypes <- list(
    "center" = double(),
    "shape" = vctrs::vec_ptype(matrix(double(), ncol = 2)),
    "inv_sqrt_shape" = vctrs::vec_ptype(matrix(double(), ncol = 2)),
    "log_volume" = double()
  )

  it("Can be built and propose points", {
    obj <- expect_all_proposals(
      new_unif_ellipsoid,
      unit_log_fn = fn,
      n_dim = 2,
      allow_failure = TRUE
    )
    expect_lrps(obj, subclass = "unif_ellipsoid", !!!ptypes)
    expect_equal(obj$enlarge, 1)
    expect_equal(obj$cache$log_volume, 0.4515827)
    expect_equal(obj$cache$inv_sqrt_shape, diag(sqrt(2 / 4), nrow = 2))
    expect_equal(obj$cache$center, c(0.5, 0.5))
  })

  it("Can be updated with a matrix of points", {
    obj <- new_unif_ellipsoid(fn, n_dim = 2)
    samples <- run_sampler(obj)
    expect_lrps(obj, subclass = "unif_ellipsoid", !!!ptypes)
    original_volume <- obj$cache$log_volume

    new_obj <- update_lrps(obj, samples$unit)
    expect_lrps(new_obj, subclass = "unif_ellipsoid", !!!ptypes)
    expect_lte(new_obj$cache$log_volume, original_volume)
    new_samples <- run_sampler(new_obj, samples$unit)

    skip_extended()
    f <- test_plot(samples$unit, new_samples$unit)
    vdiffr::expect_doppelganger("unif_ellipsoid", f)
  })

  it("Can be updated without a matrix", {
    obj <- new_unif_ellipsoid(fn, n_dim = 2)
    samples <- run_sampler(obj)
    expect_idempotent_update(
      obj,
      "unif_ellipsoid",
      ptypes = ptypes
    )
  })

  it("Throws a warning when provided with poor points", {
    obj <- new_unif_ellipsoid(fn, n_dim = 2)
    live <- matrix(rep(0.5, 500 * 2), nrow = 500)
    expect_warning(
      update_lrps(obj, live),
      "Ellipsoid fitting returned an error code"
    )
    expect_equal(obj$cache$log_volume, 0.4515827)
    expect_equal(obj$cache$inv_sqrt_shape, diag(sqrt(2 / 4), nrow = 2))
    expect_equal(obj$cache$center, c(0.5, 0.5))

    obj <- new_unif_ellipsoid(fn, n_dim = 2)
    live <- matrix(rep(0.25, 0.25, 2), nrow = 1)
    expect_warning(
      update_lrps(obj, live),
      "Ellipsoid fitting returned an error code"
    )
    expect_equal(obj$cache$log_volume, log(uniformly::volume_sphere(2)))
    expect_equal(obj$cache$center, c(0.25, 0.25))
  })
})

test_that("unif_ellipsoid can provide good results", {
  skip_extended()
  expect_gaussian_run(unif_ellipsoid())
})

describe("BoundingEllipsoid", {
  n_points <- 5000
  it("fits points in 3D correctly", {
    shape <- matrix(
      c(1.439, -1.607, 0.626, -1.607, 2.685, -0.631, 0.626, -0.631, 0.43),
      nrow = 3,
      byrow = TRUE
    )
    original_points <- uniformly::runif_in_ellipsoid(n_points, shape, 1)
    theoretical_cov <- (1 / (3 + 2)) * solve(shape)

    ell_fit <- BoundingEllipsoid(original_points, NA)
    expect_equal(ell_fit$error, 0)

    new_points <- uniformly::runif_in_sphere(n_points, 3, 1) %*%
      ell_fit$inv_sqrt_shape
    new_points <- sweep(new_points, 2, ell_fit$center, "+")

    expect_equal(colMeans(new_points), c(0, 0, 0), tolerance = 0.05)
    expect_equal(ell_fit$center, colMeans(original_points), tolerance = 0.1)

    sample_cov <- cov(original_points)
    fitted_cov <- cov(new_points)
    expect_equal(fitted_cov, theoretical_cov, tolerance = 0.1)
    expect_equal(fitted_cov, sample_cov, tolerance = 0.1)
    expect_equal(ell_fit$log_vol, 2.248886, tolerance = 0.05)
  })

  it("fits points in 5D correctly", {
    shape <- matrix(nrow = 5, ncol = 5, byrow = TRUE)
    shape[1, ] <- c(0.228, 0.0948, -0.133, -0.174, 0.00331)
    shape[2, ] <- c(0.0948, 0.174, -0.0954, -0.146, 0.00501)
    shape[3, ] <- c(-0.133, -0.0954, 0.268, -0.0323, -0.00409)
    shape[4, ] <- c(-0.174, -0.146, -0.0323, 0.386, -0.00151)
    shape[5, ] <- c(0.00331, 0.00501, -0.00409, -0.00151, 0.0678)
    shape <- 1e4 * shape
    original_points <- uniformly::runif_in_ellipsoid(n_points, shape, 1)

    theoretical_cov <- (1 / (3 + 2)) * solve(shape)

    ell_fit <- BoundingEllipsoid(original_points, NA)
    expect_false(is.infinite(ell_fit$log_vol))
    expect_equal(ell_fit$error, 0)

    new_points <- uniformly::runif_in_sphere(n_points, 5, 1) %*%
      ell_fit$inv_sqrt_shape
    expect_equal(colMeans(new_points), c(0, 0, 0, 0, 0), tolerance = 0.05)
    expect_equal(ell_fit$center, colMeans(original_points), tolerance = 0.1)

    sample_cov <- cov(original_points)
    fitted_cov <- cov(new_points)
    expect_equal(fitted_cov, theoretical_cov, tolerance = 0.05)
    expect_equal(fitted_cov, sample_cov, tolerance = 0.1)
    expect_equal(ell_fit$log_vol, -16.13215, tolerance = 0.05)
  })

  it("Recovers from degenerate live point matrices", {
    x <- runif(100)
    xy <- unname(cbind(x, 2 * x))
    ell_fit <- BoundingEllipsoid(xy, NA)
    expect_equal(ell_fit$error, 2L)
    expect_equal(ell_fit$center, c(0.5, 1), tolerance = 0.1)
  })
})
