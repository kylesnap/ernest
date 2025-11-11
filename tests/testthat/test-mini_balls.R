fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("mini_balls can be called by user", {
  expect_silent(default <- mini_balls())
  expect_snapshot(mini_balls(enlarge = 0.5), error = TRUE)
  expect_snapshot(mini_balls(enlarge = 1.0))
  expect_snapshot(mini_balls(method = "swoop"), error = TRUE)
  expect_snapshot(mini_balls(p = 0), error = TRUE)
  expect_equal(default$enlarge, 1.1)
  expect_equal(default$p, 2)
  expect_snapshot(default)
})

describe("mini_balls class", {
  obj <- new_mini_balls(fn, 2)
  it("Can be constructed with new_", {
    check_valid_lrps(
      obj,
      add_names = c("method", "p", "enlarge"),
      cache_names = "radius",
      cache_types = "double"
    )
    expect_equal(obj$cache$radius, -Inf)
  })

  it("Can call propose", {
    # Set a valid radius for proposals
    obj$cache$radius <- 0.5
    check_propose(obj, fn)
  })

  it("Can be updated", {
    # Generate some points for update
    points <- matrix(runif(20), ncol = 2)
    res <- check_update_lrps(
      obj,
      add_names = c("method", "p", "enlarge"),
      cache_names = "radius",
      cache_types = "double"
    )

    skip_snapshot()
    fig <- \() {
      plot(res$old, xlim = c(0, 1), ylim = c(0, 1))
      points(res$new, col = "red")
    }
    vdiffr::expect_doppelganger("update.mini_balls", fig)
  })
})

describe("mini_balls works with non-euclidean norms:", {
  it("manhattan", {
    obj <- mini_balls(method = "manhattan")
    expect_equal(obj$method, "manhattan")
    expect_equal(obj$p, 1)
    expect_equal(obj, mini_balls(p = 1))
    expect_snapshot(obj)

    obj <- new_mini_balls(fn, n_dim = 2, method = "manhattan", p = 1)
    obj$cache$radius <- 1
    res <- check_update_lrps(
      obj,
      add_names = c("method", "p", "enlarge"),
      cache_names = "radius",
      cache_types = "double"
    )
  })

  it("maximum", {
    obj <- mini_balls(method = "maximum")
    expect_equal(obj$method, "maximum")
    expect_equal(obj$p, Inf)
    expect_equal(obj, mini_balls(p = Inf))
    expect_snapshot(obj)

    obj <- new_mini_balls(fn, n_dim = 2, method = "maximum", p = Inf)
    obj$cache$radius <- 1
    res <- check_update_lrps(
      obj,
      add_names = c("method", "p", "enlarge"),
      cache_names = "radius",
      cache_types = "double"
    )
  })

  it("3-norm", {
    obj <- mini_balls(p = 3)
    expect_equal(obj$method, "minkowski")
    expect_equal(obj$p, 3)
    expect_snapshot(obj)

    obj <- new_mini_balls(fn, n_dim = 2, method = "minkowski", p = 3)
    obj$cache$radius <- 1
    check_update_lrps(
      obj,
      add_names = c("method", "p", "enlarge"),
      cache_names = "radius",
      cache_types = "double"
    )
  })
})

test_that("update throws a warning when the points are all identical", {
  obj <- new_mini_balls(fn, 2)
  live <- matrix(rep(0.5, 500 * 2), nrow = 500)
  expect_snapshot(update_lrps(obj, live))
  expect_equal(obj$cache$radius, -Inf)
  skip_snapshot()
  expect_snapshot(propose(obj, c(0.5, 0.5), -Inf))
})

test_that("mini_balls can provide good results", {
  run_gaussian_blobs(mini_balls(), tolerance = 2)
  run_3d(mini_balls(), tolerance = 2)
  run_eggbox(mini_balls(), tolerance = 3)
})
