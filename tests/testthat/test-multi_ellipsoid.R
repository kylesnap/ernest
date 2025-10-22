set.seed(42)

describe("MultiBoundingEllipsoids", {
  n_points <- 5000

  it("fits points in 3D correctly", {
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
    expect_named(result, c("prob", "ellipsoid"))
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
    ells <- result$ellipsoid
    print(ells)
    new_1 <- result$ellipsoid[[1]]$center +
      (uniformly::runif_in_sphere(points_per_k, n_dim) %*% ells[[1]]$sqrt_shape)
    new_2 <- result$ellipsoid[[2]]$center +
      (uniformly::runif_in_sphere(points_per_k, n_dim) %*% ells[[2]]$sqrt_shape)
    new_3 <- result$ellipsoid[[3]]$center +
      (uniformly::runif_in_sphere(points_per_k, n_dim) %*% ells[[3]]$sqrt_shape)
    new_all <- rbind(new_1, new_2, new_3)

    skip_on_cran()
    skip_extended_test()
    vdiffr::expect_doppelganger("multi_ellipsoid", {
      plot(data_all)
      points(new_all, col = "red")
    })
  })
})

test_that("Holder", {})
