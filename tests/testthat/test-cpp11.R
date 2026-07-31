run_cpp_tests("ernest")
withr::local_seed(42)
test_that("D", {})

describe("BoundingEllipsoid", {
  nlive <- 5000
  it("fits points in 3D correctly", {
    shape <- matrix(
      c(1.439, -1.607, 0.626, -1.607, 2.685, -0.631, 0.626, -0.631, 0.43),
      nrow = 3,
      byrow = TRUE
    )
    original_points <- readRDS(test_path("3d_ellipsoid.rds"))
    # original_points <- runif_in_ellipsoid(nlive, shape, 1)
    theoretical_cov <- (1 / (3 + 2)) * solve(shape)

    ell_fit <- BoundingEllipsoid(original_points, NA)
    new_points <- runif_in_sphere(nlive, 3, 1) %*%
      ell_fit$inv_sqrt_shape
    new_points <- sweep(new_points, 2, ell_fit$center, "+")
    expect_equal(colMeans(new_points), c(0, 0, 0), tolerance = 0.05)

    sample_cov <- cov(original_points)
    fitted_cov <- cov(new_points)
    expect_equal(fitted_cov, theoretical_cov, tolerance = 0.1)
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
    original_points <- readRDS(test_path("5d_ellipsoid.rds"))
    # original_points <- runif_in_ellipsoid(nlive, shape, 1)

    theoretical_cov <- (1 / (3 + 2)) * solve(shape)

    ell_fit <- BoundingEllipsoid(original_points, NA)
    new_points <- runif_in_sphere(nlive, 5, 1) %*%
      ell_fit$inv_sqrt_shape
    expect_equal(colMeans(new_points), c(0, 0, 0, 0, 0), tolerance = 0.05)

    sample_cov <- cov(original_points)
    fitted_cov <- cov(new_points)
    expect_equal(fitted_cov, theoretical_cov, tolerance = 0.05)
    expect_equal(ell_fit$log_vol, -16.13215, tolerance = 0.05)
  })

  it("Recovers from degenerate live point matrices", {
    x <- runif(100)
    xy <- unname(cbind(x, 2 * x))
    ell_fit <- BoundingEllipsoid(xy, NA)
    expect_equal(ell_fit$error, 2L)
    expect_equal(ell_fit$center, c(0.5, 1), tolerance = 0.1)
  })

  it("Recovers from fitting with a single live point", {
    x <- matrix(c(0.25, 0.75), nrow = 1)
    ell_fit <- BoundingEllipsoid(x, NA)
    expect_equal(ell_fit$center, c(0.25, 0.75))
  })
})

describe("MultiBoundingEllipsoids", {
  it("fits points in 3D correctly", {
    nlive <- 2000
    shape <- matrix(
      c(1.439, -1.607, 0.626, -1.607, 2.685, -0.631, 0.626, -0.631, 0.43),
      nrow = 3,
      byrow = TRUE
    )
    original_points <- readRDS(test_path("3d_ellipsoid.rds"))
    theoretical_cov <- (1 / (3 + 2)) * solve(shape)

    ell_fit <- MultiBoundingEllipsoids(original_points, NA)

    el <- ell_fit$ellipsoid[[1]]
    new_points <- runif_in_sphere(2000, 3, 1) %*% el$inv_sqrt_shape
    new_points <- sweep(new_points, 2, el$center, "+")

    skip_extended()
    f <- test_plot(original_points, new_points)
    vdiffr::expect_doppelganger("BoundingEllipsoids in 3D", f)
  })

  it("recovers clusters in torus", {
    skip_extended()
    n <- 1000
    R <- 1.0
    r <- 0.1
    # original_points <- runif_in_torus(n, R, r)
    original_points <- readRDS(test_path("torus.rds"))
    point_log_volume <- -8.530319 # log(volume_torus(R, r) / n)
    el <- MultiBoundingEllipsoids(original_points, point_log_volume)

    f <- \() {
      plot(original_points)
      for (e in el$ellipsoid) {
        new_points <- runif_in_sphere(200, 3, 1) %*%
          e$inv_sqrt_shape
        new_points <- sweep(new_points, 2, e$center, "+")
        points(new_points, col = "red")
      }
    }
    vdiffr::expect_doppelganger("BoundingEllipsoids torus", f)
  })

  it("recovers correct number of clusters in grid", {
    skip_extended()
    nvar <- 4
    nxcens <- 4
    ncens <- nxcens^nvar
    sig <- 0.01
    threshold <- 0.1

    npt <- ncens * 10 * nvar
    cens <- as.matrix(expand.grid(rep(list(seq_len(nxcens)), nvar)))
    xs <- matrix(rnorm(npt * nvar, sd = sig), ncol = nvar) +
      cens[(seq_len(npt) - 1) %% nrow(cens) + 1, ]

    ell_fit <- MultiBoundingEllipsoids(xs, NA)
    n_ell <- length(ell_fit$ellipsoid)
    expect_lt(abs(n_ell / ncens - 1), threshold)
  })
})
