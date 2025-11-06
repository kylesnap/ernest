fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("slice can be called by user", {
  default <- slice_rectangle()
  expect_snapshot(slice_rectangle(enlarge = 0.5), error = TRUE)
  expect_snapshot(slice_rectangle(enlarge = NA))
  expect_equal(default$enlarge, 1)
  expect_snapshot(default)
})

describe("slice class", {
  obj <- new_slice_rectangle(fn, 2)
  it("Can be constructed with new_", {
    check_valid_lrps(
      obj,
      add_names = "enlarge",
      cache_names = c("lower", "upper", "n_accept"),
      cache_types = c("double", "double", "integer")
    )
    expect_equal(obj$enlarge, 1.25)
    expect_equal(obj$cache$lower, c(0, 0))
    expect_equal(obj$cache$upper, c(1, 1))
    expect_snapshot(obj)
  })

  it("Can call propose", {
    check_propose(obj, fn, fail_on_no_accept = FALSE)
  })

  it("Can be updated", {
    res <- check_update_lrps(
      obj,
      add_names = "enlarge",
      cache_names = c("lower", "upper", "n_accept"),
      cache_types = c("double", "double", "integer")
    )

    skip_snapshot()
    fig <- \() {
      plot(res$old, xlim = c(0, 1), ylim = c(0, 1))
      points(res$new, col = "red")
    }
    vdiffr::expect_doppelganger("update.slice", fig)
  })
})

test_that("slice can provide good results", {
  run_gaussian_blobs(slice_rectangle(), tolerance = 2)
  run_3d(slice_rectangle(), tolerance = 2)
  run_eggbox(slice_rectangle(), tolerance = 2)
})
