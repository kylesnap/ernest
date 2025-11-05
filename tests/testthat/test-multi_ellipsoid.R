set.seed(42)
fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()

test_that("multi_ellipsoid can be called by user", {
  default <- multi_ellipsoid()
  expect_snapshot(multi_ellipsoid(enlarge = 0.5), error = TRUE)
  expect_snapshot(multi_ellipsoid(enlarge = 1))
  expect_snapshot(multi_ellipsoid(min_reduction = 1))
  expect_snapshot(multi_ellipsoid(min_reduction = -0.1), error = TRUE)
  expect_snapshot(multi_ellipsoid(min_reduction = 1.1), error = TRUE)
  expect_snapshot(multi_ellipsoid(allow_contact = "boop"), error = TRUE)
  expect_snapshot(default)
  expect_equal(default$enlarge, 1.25)
  expect_equal(default$min_reduction, 0.7)
  expect_true(default$allow_contact)
})

describe("multi_ellipsoid class", {
  obj <- new_multi_ellipsoid(fn, n_dim = 2, min_reduction = 0.9)
  it("Can be constructed with new_", {
    check_valid_lrps(
      obj,
      add_names = c("enlarge", "min_reduction", "allow_contact"),
      cache_names = c("prob", "ellipsoid", "total_log_volume"),
      cache_types = c("double", "list", "double")
    )
    expect_equal(obj$enlarge, 1)
    expect_equal(obj$min_reduction, 0.9)
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
      add_names = c("enlarge", "min_reduction", "allow_contact"),
      cache_names = c("prob", "ellipsoid", "total_log_volume"),
      cache_types = c("double", "list", "double")
    )

    skip_on_cran()
    skip_extended_test()
    fig <- \() {
      plot(res$old, xlim = c(0, 1), ylim = c(0, 1))
      points(res$new, col = "red")
    }
    vdiffr::expect_doppelganger("update.multi_ellipsoid", fig)
  })
})

test_that("multi_ellipsoid can provide good results", {
  run_gaussian_blobs(multi_ellipsoid())
  run_3d(multi_ellipsoid(), tolerance = 2)
  run_eggbox(multi_ellipsoid(min_reduction = 0.5), tolerance = 2)
})

test_that("MultiBoundingEllipsoids fits points in 3D correctly", {
  n_points <- 5000
  n_dim <- 3L
  k <- 3L
  points_per_k <- n_points %/% k

  data_1 <- -5 + uniformly::runif_in_sphere(points_per_k, n_dim)
  data_2 <- uniformly::runif_in_sphere(points_per_k, n_dim)
  data_3 <- 5 + uniformly::runif_in_sphere(points_per_k, n_dim)
  data_all <- rbind(data_1, data_2, data_3)

  result <- MultiBoundingEllipsoids(
    data_all,
    min_reduction = 0.7,
    allow_contact = TRUE
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("prob", "ellipsoid", "tot_log_vol"))
  expect_equal(sum(result$prob), 1)

  # Expect three ellipsoids
  expect_equal(length(result$prob), 3)
  centers <- lapply(result$ellipsoid, function(x) x[["center"]])
  expect_equal(
    sort(list_c(centers)),
    c(-5, -5, -5, 0, 0, 0, 5, 5, 5),
    tolerance = 0.01
  )

  # Expect similar with allow_contact FALSE
  result <- MultiBoundingEllipsoids(
    data_all,
    min_reduction = 1,
    allow_contact = FALSE
  )
  expect_equal(length(result$prob), 3)
  centers <- lapply(result$ellipsoid, function(x) x[["center"]])
  expect_equal(
    sort(list_c(centers)),
    c(-5, -5, -5, 0, 0, 0, 5, 5, 5),
    tolerance = 0.01
  )

  # Plot Test Points
  skip_on_cran()
  skip_extended_test()
  ells <- result$ellipsoid
  new_1 <- ells[[1]]$center +
    (uniformly::runif_in_sphere(points_per_k, n_dim) %*%
      ells[[1]]$inv_sqrt_shape)
  new_2 <- ells[[2]]$center +
    (uniformly::runif_in_sphere(points_per_k, n_dim) %*%
      ells[[2]]$inv_sqrt_shape)
  new_3 <- ells[[3]]$center +
    (uniformly::runif_in_sphere(points_per_k, n_dim) %*%
      ells[[3]]$inv_sqrt_shape)
  new_all <- rbind(new_1, new_2, new_3)

  fig <- \() {
    plot(data_all)
    points(new_all, col = "red")
  }
  vdiffr::expect_doppelganger("multi_ellipsoid", fig)
})

test_that("update throws a warning when the points are all identical", {
  obj <- new_multi_ellipsoid(fn, 2)
  live <- matrix(rep(0.5, 500 * 2), nrow = 500)
  expect_snapshot(update_lrps(obj, live))
  expect_equal(obj$enlarge, 1)
  expect_equal(obj$min_reduction, 0.7)
  expect_equal(obj$cache$ellipsoid[[1]]$log_vol, 0.4515827)
  expect_equal(
    obj$cache$ellipsoid[[1]]$inv_sqrt_shape,
    diag(sqrt(2 / 4), nrow = 2)
  )
})
