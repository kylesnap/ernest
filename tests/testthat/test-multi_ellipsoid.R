fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

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

test_that("multi_ellipsoid returns correct class and structure", {
  obj <- multi_ellipsoid(
    enlarge = 1.5,
    min_reduction = 0.6,
    allow_contact = FALSE
  )
  expect_s3_class(obj, c("multi_ellipsoid", "ernest_lrps"), exact = TRUE)
  expect_null(obj$unit_log_fn)
  expect_null(obj$n_dim)
  expect_equal(obj$max_loop, 1e6L)
  expect_equal(obj$enlarge, 1.5)
  expect_equal(obj$min_reduction, 0.6)
  expect_equal(obj$allow_contact, FALSE)
  expect_mapequal(as.list(obj$cache), list(n_call = 0L))

  obj <- multi_ellipsoid()
  expect_equal(obj$enlarge, 1.25)
  expect_equal(obj$min_reduction, 0.7)
  expect_equal(obj$allow_contact, TRUE)
})

describe("new_multi_ellipsoid", {
  it("fails when parameters are improper", {
    expect_snapshot(new_multi_ellipsoid(fn, 2L, enlarge = 0.5), error = TRUE)
    expect_snapshot(
      new_multi_ellipsoid(fn, 2L, min_reduction = 1.5),
      error = TRUE
    )
    expect_snapshot(
      new_multi_ellipsoid(fn, 2L, min_reduction = -0.1),
      error = TRUE
    )
  })

  it("passes the correct defaults", {
    obj <- new_multi_ellipsoid(fn, 2, min_reduction = 0.7, allow_contact = TRUE)
    expect_s3_class(obj, c("multi_ellipsoid", "ernest_lrps"), exact = TRUE)
    expect_equal(obj$unit_log_fn, fn)
    expect_equal(obj$n_dim, 2)
    expect_equal(obj$max_loop, 1e6L)
    expect_equal(obj$enlarge, 1.0)
    expect_equal(obj$min_reduction, 0.7)
    expect_equal(obj$allow_contact, TRUE)

    cache_contents <- obj$cache
    expect_named(
      obj$cache,
      c("prob", "ellipsoid", "total_log_volume", "n_call")
    )
    expect_mapequal(
      obj$cache$ellipsoid[[1]],
      list(
        center = c(0.5, 0.5),
        shape = diag(2, nrow = 2),
        inv_sqrt_shape = diag(sqrt(2 / 4), nrow = 2),
        log_vol = 0.4515827,
        error = 0
      )
    )
    expect_snapshot(obj)
  })
})

multi <- new_multi_ellipsoid(
  fn,
  n_dim = 2,
  enlarge = 1.2,
  min_reduction = 0.9,
  allow_contact = TRUE
)
describe("propose.multi_ellipsoid", {
  it("proposes points in the unit cube", {
    result <- propose(multi, original = NULL, criterion = -Inf)
    expect_length(result$unit, 2)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_equal(multi$cache$n_call, 0L)
  })

  it("proposes points from the ellipsoid", {
    result <- propose(multi, original = c(0.5, 0.5), criterion = -99.3068)
    expect_length(result$unit, 2)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_type(result$ellipsoid_idx, "integer")
  })
})

live <- replicate(
  500,
  propose(multi, original = c(0.5, 0.5), criterion = -99.3068)
)
live <- do.call(rbind, live["unit", ])
describe("update_lrps.multi_ellipsoid", {
  it("is idempotent", {
    new_multi <- update_lrps(multi)
    expect_identical(new_multi, multi)
  })

  it("can rebound to a matrix of live points", {
    new_multi <- update_lrps(multi, live)
    ellipsoids <- new_multi$cache$ellipsoid
    expect_length(ellipsoids, 2)
    expect_equal(sum(new_multi$cache$prob), 1.0)

    new_live <- replicate(
      500,
      propose(new_multi, original = c(0.5, 0.5), criterion = -Inf)
    )
    new_live <- do.call(rbind, new_live["unit", ])
    new_idx <- list_c(new_live["ellipsoid_idx"])

    # Check that all new points fall within at least one enlarged ellipsoid
    n_oob <- 0
    for (j in seq_len(500)) {
      point_in_any <- FALSE
      for (ell in ellipsoids) {
        d <- new_live[j, ] - ell$center
        dist_sq <- drop(d %*% ell$shape %*% d)
        if (dist_sq <= new_multi$enlarge) {
          point_in_any <- TRUE
          break
        }
      }
      if (!point_in_any) n_oob <- n_oob + 1
    }
    expect_equal(n_oob, 0)
    expect_snapshot(new_multi)

    skip_extended_test()
    skip_on_cran()
    fig <- \(x) {
      plot(
        live,
        xlim = c(0, 1),
        ylim = c(0, 1),
        main = "Multi-Ellipsoid Sampling"
      )
      points(new_live, col = "red", pch = 20, cex = 0.5)
    }
    vdiffr::expect_doppelganger("propose.multi_ellipsoid", fig)
  })

  it("reports numerical errors", {
    x <- seq(0.01, 0.49, length.out = 500)
    xy <- cbind(x, x * 2)
    expect_snapshot(new_multi <- update_lrps(multi, xy))
  })
})
