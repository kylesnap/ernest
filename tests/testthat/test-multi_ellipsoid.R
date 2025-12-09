set.seed(42)
fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()

test_that("multi_ellipsoid can be called by user", {
  default <- multi_ellipsoid()
  expect_snapshot(multi_ellipsoid(enlarge = 0.5), error = TRUE)
  expect_snapshot(multi_ellipsoid(enlarge = 1))
  expect_snapshot(default)
  expect_equal(default$enlarge, 1.25)
})

describe("multi_ellipsoid class", {
  obj <- new_multi_ellipsoid(fn, n_dim = 2)
  it("Can be constructed with new_", {
    check_valid_lrps(
      obj,
      add_names = "enlarge",
      cache_names = c("prob", "ellipsoid", "total_log_volume"),
      cache_types = c("double", "list", "double")
    )
    expect_equal(obj$enlarge, 1)
    expect_equal(obj$cache$ellipsoid[[1]]$log_vol, 0.4515827)
    expect_equal(
      obj$cache$ellipsoid[[1]]$inv_sqrt_shape,
      diag(sqrt(2 / 4), nrow = 2)
    )
  })

  it("Can call propose", {
    check_propose(obj, fn)
  })

  it("Can be updated", {
    res <- check_update_lrps(
      obj,
      add_names = "enlarge",
      cache_names = c("prob", "ellipsoid", "total_log_volume"),
      cache_types = c("double", "list", "double")
    )

    skip_snapshot()
    fig <- \() {
      plot(res$old, xlim = c(0, 1), ylim = c(0, 1))
      points(res$new, col = "red")
    }
    vdiffr::expect_doppelganger("update.multi_ellipsoid", fig)
  })
})

test_that("multi_ellipsoid can provide good results", {
  run_gaussian_blobs(multi_ellipsoid(), tolerance = 3)
  run_3d(multi_ellipsoid(), tolerance = 2)
  run_eggbox(multi_ellipsoid(), tolerance = 2)
})

test_that("MultiBoundingEllipsoids fits points in 3D correctly", {
  n_points <- 2000
  shape <- matrix(
    c(1.439, -1.607, 0.626, -1.607, 2.685, -0.631, 0.626, -0.631, 0.43),
    nrow = 3,
    byrow = TRUE
  )
  original_points <- uniformly::runif_in_ellipsoid(n_points, shape, 1)
  theoretical_cov <- (1 / (3 + 2)) * solve(shape)

  ell_fit <- MultiBoundingEllipsoids(original_points, NA)

  expect_length(ell_fit$prob, 1)
  el <- ell_fit$ellipsoid[[1]]
  expect_equal(el$log_vol, ell_fit$tot_log_vol)
  new_points <- uniformly::runif_in_sphere(2000, 3, 1) %*% el$inv_sqrt_shape
  new_points <- sweep(new_points, 2, el$center, "+")

  fig <- \() {
    plot(original_points)
    points(new_points, col = "red")
  }
  vdiffr::expect_doppelganger("multi_ellipsoid simple", fig)
})

test_that("MultiBoundingEllipsoids recovers clusters in torus", {
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
  vdiffr::expect_doppelganger("multi_ellipsoid torus", f)
})

test_that("MultiBoundingEllipsoids recovers correct number of clusters in grid", {
  ndim <- 4
  nxcens <- 4
  ncens <- nxcens^ndim
  sig <- 0.01
  threshold <- 0.1

  npt <- ncens * 10 * ndim
  # Create grid centers
  cens <- as.matrix(expand.grid(rep(list(seq_len(nxcens)), ndim)))
  # Generate points around grid centers
  xs <- matrix(rnorm(npt * ndim, sd = sig), ncol = ndim) +
    cens[(seq_len(npt) - 1) %% nrow(cens) + 1, ]

  ell_fit <- MultiBoundingEllipsoids(xs, NA)
  n_ell <- length(ell_fit$ellipsoid)
  expect_lt(abs(n_ell / ncens - 1), threshold)
})

test_that("update throws a warning when the points are all identical", {
  obj <- new_multi_ellipsoid(fn, 2)
  live <- matrix(rep(0.5, 500 * 2), nrow = 500)
  expect_snapshot(update_lrps(obj, live))
  expect_equal(obj$enlarge, 1)
  expect_equal(obj$cache$ellipsoid[[1]]$log_vol, 0.4515827)
  expect_equal(
    obj$cache$ellipsoid[[1]]$inv_sqrt_shape,
    diag(sqrt(2 / 4), nrow = 2)
  )
})
