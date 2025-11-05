fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

test_that("unif_cube returns correct class and structure", {
  default <- unif_cube()
  expect_equal(default$max_loop, 1e6L)
  expect_snapshot(default)
})

describe("unif_cube class", {
  obj <- new_unif_cube(fn, 2L, max_loop = 100)
  it("Can be constructed with new_", {
    check_valid_lrps(obj)
    expect_equal(obj$max_loop, 100)
    expect_equal(obj$n_dim, 2L)
  })

  it("Can call propose", {
    check_propose(obj, fn)
  })

  it("Can be updated", {
    res <- check_update_lrps(obj)
    skip_on_cran()
    skip_extended_test()
    fig <- \() {
      plot(res$old, xlim = c(0, 1), ylim = c(0, 1))
      points(res$new, col = "red")
    }
    vdiffr::expect_doppelganger("update.unif_cube", fig)
  })
})

test_that("unif_cube can provide good results", {
  # Only testing blobs
  run_gaussian_blobs(unif_cube())
})
