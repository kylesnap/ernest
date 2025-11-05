fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("rwmh_cube can be called by user", {
  default <- rwmh_cube()
  expect_snapshot(rwmh_cube(steps = 1), error = TRUE)
  expect_snapshot(rwmh_cube(target_acceptance = 0.01), error = TRUE)
  expect_snapshot(rwmh_cube(target_acceptance = 1.1), error = TRUE)
  expect_equal(default$steps, 25L)
  expect_equal(default$target_acceptance, 0.5)
  expect_snapshot(default)
})

describe("rwmh_cube class", {
  obj <- new_rwmh_cube(fn, 2)
  it("Can be constructed with new_", {
    check_valid_lrps(
      obj,
      add_names = c("steps", "target_acceptance"),
      cache_names = c("n_accept", "epsilon"),
      cache_types = c("integer", "double")
    )
    expect_equal(obj$cache$epsilon, 1.0)
  })

  it("Can call propose", {
    check_propose(obj, fn, fail_on_no_accept = FALSE)
  })

  it("Can be updated", {
    res <- check_update_lrps(
      obj,
      add_names = c("steps", "target_acceptance"),
      cache_names = c("n_accept", "epsilon"),
      cache_types = c("integer", "double")
    )

    skip_on_cran()
    skip_extended_test()
    fig <- \() {
      plot(res$old, xlim = c(0, 1), ylim = c(0, 1))
      points(res$new, col = "red")
    }
    vdiffr::expect_doppelganger("update.rwmh_cube", fig)
  })
})

test_that("rwmh_cube can provide good results", {
  run_gaussian_blobs(rwmh_cube())
  run_3d(rwmh_cube())
  run_eggbox(rwmh_cube())
})
