fn <- \(x) gaussian_blobs$log_lik(gaussian_blobs$prior$fn(x))
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("adaptive_rwmh can be called by user", {
  default <- adaptive_rwmh()
  expect_snapshot(adaptive_rwmh(steps = 1), error = TRUE)
  expect_snapshot(adaptive_rwmh(target_acceptance = 1.1), error = TRUE)
  expect_snapshot(adaptive_rwmh(target_acceptance = 0.02), error = TRUE)
  expect_snapshot(adaptive_rwmh(min_epsilon = -0.1), error = TRUE)
  expect_snapshot(adaptive_rwmh(strength = -5), error = TRUE)
  expect_snapshot(adaptive_rwmh(forgetfulness = -0.1), error = TRUE)
  expect_snapshot(adaptive_rwmh(forgetfulness = 1.1), error = TRUE)
  expect_equal(default$steps, 25L)
  expect_equal(default$target_acceptance, 0.4)
  expect_equal(default$min_epsilon, 0.1)
  expect_null(default$strength, 0.1)
  expect_equal(default$forgetfulness, 0.4)
  expect_snapshot(default)
})

describe("adaptive_rwmh class", {
  obj <- new_adaptive_rwmh(fn, 2)
  it("Can be constructed with new_", {
    check_valid_lrps(
      obj,
      add_names = c(
        "steps",
        "target_acceptance",
        "min_epsilon",
        "strength",
        "forgetfulness"
      ),
      cache_names = c("mean", "covariance", "epsilon", "n_accept"),
      cache_types = c("double", "double", "double", "integer")
    )
    expect_equal(obj$cache$epsilon, 1)
    expect_equal(obj$cache$mean, c(0.5, 0.5))
    expect_equal(obj$cache$covariance, diag(1 / 12, nrow = 2))
  })

  it("Can call propose", {
    check_propose(obj, fn, fail_on_no_accept = FALSE)
  })

  it("Can be updated", {
    res <- check_update_lrps(
      obj,
      add_names = c(
        "steps",
        "target_acceptance",
        "min_epsilon",
        "strength",
        "forgetfulness"
      ),
      cache_names = c("mean", "covariance", "epsilon", "n_accept"),
      cache_types = c("double", "double", "double", "integer")
    )

    skip_on_cran()
    skip_extended_test()
    fig <- \() {
      plot(res$old, xlim = c(0, 1), ylim = c(0, 1))
      points(res$new, col = "red")
    }
    vdiffr::expect_doppelganger("update.adaptive_rwmh", fig)
  })
})
