fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("multi_ellipsoid can be called by user", {
  default <- multi_ellipsoid()
  expect_snapshot(multi_ellipsoid(enlarge = 0.5), error = TRUE)
  expect_snapshot(multi_ellipsoid(enlarge = 1))
  expect_snapshot(default)
  expect_equal(default$enlarge, 1.25)
})

describe("multi_ellipsoid class", {
  ptypes <- list(
    "ellipsoid" = list(),
    "prob" = double(),
    "total_log_volume" = double()
  )

  it("Can be build and propose points", {
    obj <- expect_all_proposals(
      new_multi_ellipsoid,
      unit_log_fn = fn,
      n_dim = 2,
      extra_args = "ellipsoid_idx",
      allow_failure = TRUE
    )
    expect_lrps(obj, subclass = "multi_ellipsoid", !!!ptypes)
  })

  it("Can be updated with a matrix of points", {
    obj <- new_multi_ellipsoid(fn, n_dim = 2)
    samples <- run_sampler(obj)

    new_obj <- update_lrps(obj, samples$unit)
    expect_lrps(new_obj, subclass = "multi_ellipsoid", !!!ptypes)
    new_samples <- run_sampler(new_obj)

    skip_extended()
    f <- test_plot(samples$unit, new_samples$unit)
    vdiffr::expect_doppelganger("multi_ellipsoid", f)
  })

  it("Can be updated without a matrix", {
    obj <- new_multi_ellipsoid(fn, n_dim = 2)
    samples <- run_sampler(obj)
    expect_idempotent_update(
      obj,
      "multi_ellipsoid",
      ptypes = ptypes
    )
  })

  it("warns when updated with an poor set of points", {
    obj <- new_multi_ellipsoid(fn, 2)
    live <- matrix(rep(0.5, 500 * 2), nrow = 500)
    expect_warning(
      update_lrps(obj, live),
      "Multi-ellipsoid fitting returned an error code"
    )
    expect_equal(obj$enlarge, 1)
    expect_equal(obj$cache$ellipsoid[[1]]$log_vol, 0.4515827)
    expect_equal(
      obj$cache$ellipsoid[[1]]$inv_sqrt_shape,
      diag(sqrt(2 / 4), nrow = 2)
    )
  })
})

test_that("multi_ellipsoid can provide good results", {
  skip_extended()
  expect_gaussian_run(multi_ellipsoid())
  expect_3D_run(multi_ellipsoid())
  expect_eggbox_run(multi_ellipsoid())
})

describe("MultiBoundingEllipsoids", {
  it("fits points in 3D correctly", {
    nlive <- 2000
    shape <- matrix(
      c(1.439, -1.607, 0.626, -1.607, 2.685, -0.631, 0.626, -0.631, 0.43),
      nrow = 3,
      byrow = TRUE
    )
    original_points <- uniformly::runif_in_ellipsoid(nlive, shape, 1)
    theoretical_cov <- (1 / (3 + 2)) * solve(shape)

    ell_fit <- MultiBoundingEllipsoids(original_points, NA)

    expect_length(ell_fit$prob, 1)
    el <- ell_fit$ellipsoid[[1]]
    expect_equal(el$log_vol, ell_fit$tot_log_vol)
    new_points <- uniformly::runif_in_sphere(2000, 3, 1) %*% el$inv_sqrt_shape
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
    original_points <- uniformly::runif_in_torus(n, R, r)
    point_log_volume <- log(uniformly::volume_torus(R, r) / n)
    el <- MultiBoundingEllipsoids(original_points, point_log_volume)

    f <- \() {
      plot(original_points)
      for (e in el$ellipsoid) {
        new_points <- uniformly::runif_in_sphere(200, 3, 1) %*%
          e$inv_sqrt_shape
        new_points <- sweep(new_points, 2, e$center, "+")
        points(new_points, col = "red")
      }
    }
    vdiffr::expect_doppelganger("BoundingEllipsoids torus", f)
  })

  it("recovers correct number of clusters in grid", {
    skip_extended()
    ndim <- 4
    nxcens <- 4
    ncens <- nxcens^ndim
    sig <- 0.01
    threshold <- 0.1

    npt <- ncens * 10 * ndim
    cens <- as.matrix(expand.grid(rep(list(seq_len(nxcens)), ndim)))
    xs <- matrix(rnorm(npt * ndim, sd = sig), ncol = ndim) +
      cens[(seq_len(npt) - 1) %% nrow(cens) + 1, ]

    ell_fit <- MultiBoundingEllipsoids(xs, NA)
    n_ell <- length(ell_fit$ellipsoid)
    expect_lt(abs(n_ell / ncens - 1), threshold)
  })
})
