fn <- \(x) gaussian_blobs$log_lik(gaussian_blobs$prior$fn(x))
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("nurs can be called by user", {
  default <- nurs(adaptive_scale = 0.1)
  expect_snapshot(nurs(adaptive_scale = -0.1), error = TRUE)
  expect_snapshot(nurs(adaptive_scale = 1.1), error = TRUE)
  expect_snapshot(nurs(fixed_scale = 0), error = TRUE)
  expect_snapshot(nurs(adaptive_scale = 0.1, fixed_scale = 0.3), error = TRUE)
  expect_snapshot(nurs(adaptive_scale = 0.1, steps = 0), error = TRUE)
  expect_snapshot(nurs(adaptive_scale = 0.1, max_orbits = 0), error = TRUE)
  expect_equal(default$steps, 3L)
  expect_equal(default$adaptive_scale, 0.1)
  expect_null(default$fixed_scale)
  expect_snapshot(default)
})

describe("nurs class", {
  obj <- new_nurs(fn, 2, adaptive_scale = 0.01)
  it("Can be constructed with new_", {
    check_valid_lrps(
      obj,
      add_names = c(
        "fixed_scale",
        "adaptive_scale",
        "steps",
        "max_orbits"
      ),
      cache_names = c("epsilon", "n_cmp", "n_accept"),
      cache_types = c("double", "integer", "integer")
    )
    expect_equal(obj$cache$epsilon, 0.01)
  })

  it("Can call propose", {
    check_propose(obj, fn, fail_on_no_accept = FALSE)
  })

  it("Can be updated", {
    res <- check_update_lrps(
      obj,
      add_names = c(
        "fixed_scale",
        "adaptive_scale",
        "steps",
        "max_orbits"
      ),
      cache_names = c("epsilon", "n_cmp", "n_accept"),
      cache_types = c("double", "integer", "integer")
    )

    skip_snapshot()
    fig <- \() {
      plot(res$old, xlim = c(0, 1), ylim = c(0, 1))
      points(res$new, col = "red")
    }
    vdiffr::expect_doppelganger("update.nurs", fig)
  })
})

describe("nurs with fixed scale", {
  obj <- new_nurs(fn, 2, fixed_scale = 0.01)
  expect_snapshot(obj)
  it("Can be constructed with new_", {
    check_valid_lrps(
      obj,
      add_names = c(
        "fixed_scale",
        "adaptive_scale",
        "steps",
        "max_orbits"
      ),
      cache_names = c("epsilon", "n_cmp", "n_accept"),
      cache_types = c("double", "integer", "integer")
    )
    expect_equal(obj$cache$epsilon, 0.01)
  })

  it("Can call propose", {
    check_propose(obj, fn, fail_on_no_accept = FALSE)
  })

  it("Can be updated", {
    res <- check_update_lrps(
      obj,
      add_names = c(
        "fixed_scale",
        "adaptive_scale",
        "steps",
        "max_orbits"
      ),
      cache_names = c("epsilon", "n_cmp", "n_accept"),
      cache_types = c("double", "integer", "integer")
    )

    skip_snapshot()
    fig <- \() {
      plot(res$old, xlim = c(0, 1), ylim = c(0, 1))
      points(res$new, col = "red")
    }
    vdiffr::expect_doppelganger("update-nurs-fixed", fig)
  })
})

test_that("nurs can provide good results", {
  # TODO
  skip()
})

test_that("Errors in distance recalculations are handled", {
  obj <- new_nurs(fn, 2, adaptive_scale = 0.01)
  env_poke(obj$cache, "epsilon", 0.005)
  expect_snapshot(new_obj <- update_lrps(obj, unit = matrix()))
  expect_equal(obj$cache$epsilon, 0.01)
})
