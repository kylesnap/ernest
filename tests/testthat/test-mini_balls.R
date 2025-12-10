fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("mini_balls can be called by user", {
  expect_silent(default <- mini_balls())
  expect_snapshot(mini_balls(method = "swoop"), error = TRUE)
  expect_snapshot(mini_balls(bootstrap = -1L), error = TRUE)
  expect_equal(default$method, "euclidean")
  expect_null(default$bootstrap)
  expect_snapshot(default)
})

describe("mini_balls class", {
  obj <- new_mini_balls(fn, n_dim = 2)
  it("Can be constructed with new_", {
    check_valid_lrps(
      obj,
      add_names = c("method", "bootstrap"),
      cache_names = c("radius", "annoy"),
      cache_types = c("double", "S4")
    )
    expect_equal(obj$cache$radius, -Inf)
  })

  it("Can be updated", {
    # Generate some points for update
    points <- matrix(runif(20), ncol = 2)
    res <- check_update_lrps(
      obj,
      add_names = c("method", "bootstrap"),
      cache_names = c("radius", "annoy"),
      cache_types = c("double", "S4")
    )

    skip_snapshot()
    fig <- \() {
      plot(res$old, xlim = c(0, 1), ylim = c(0, 1))
      points(res$new, col = "red")
    }
    vdiffr::expect_doppelganger("update.mini_balls euclidean", fig)
  })
})

describe("mini_balls works with non-defaults:", {
  it("bootstrapped euclidean", {
    obj <- mini_balls(bootstrap = 30)
    expect_identical(obj$bootstrap, 30L)
    expect_snapshot(obj)

    obj <- new_mini_balls(fn, n_dim = 2, method = "maximum")
    obj$cache$radius <- 1
    res <- check_update_lrps(
      obj,
      add_names = c("method", "bootstrap"),
      cache_names = c("radius", "annoy"),
      cache_types = c("double", "S4")
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
