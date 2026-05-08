fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
withr::local_seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("multi_ellipsoid can be called by user", {
  default <- multi_ellipsoid()
  expect_snapshot(multi_ellipsoid(enlarge = 0.5), error = TRUE)
  expect_snapshot(multi_ellipsoid(enlarge = 1))
  expect_snapshot(default)
})

describe("multi_ellipsoid class", {
  ptypes <- list(
    "ellipsoid" = list(),
    "prob" = double(),
    "total_log_volume" = double()
  )

  it("Can be build and propose points", {
    obj <- expect_all_proposals(
      new_multi_ellipsoid,
      unit_log_fn = fn,
      nvar = 2,
      extra_args = "ellipsoid_idx",
      allow_failure = TRUE
    )
    expect_lrps(obj, subclass = "multi_ellipsoid", !!!ptypes)
  })

  it("Can be updated with a matrix of points", {
    obj <- new_multi_ellipsoid(fn, nvar = 2)
    samples <- run_sampler(obj)

    new_obj <- update_lrps(obj, samples$unit)
    expect_lrps(new_obj, subclass = "multi_ellipsoid", !!!ptypes)
    new_samples <- run_sampler(new_obj)

    skip_extended()
    f <- test_plot(samples$unit, new_samples$unit)
    vdiffr::expect_doppelganger("multi_ellipsoid", f)
  })

  it("Can be updated without a matrix", {
    obj <- new_multi_ellipsoid(fn, nvar = 2)
    samples <- run_sampler(obj)
    expect_idempotent_update(
      obj,
      "multi_ellipsoid",
      ptypes = ptypes
    )
  })

  it("warns when updated with an poor set of points", {
    obj <- new_multi_ellipsoid(fn, 2)
    live <- matrix(rep(0.5, 500 * 2), nrow = 500)
    expect_warning(
      update_lrps(obj, live),
      "Multi-ellipsoid fitting returned an error code"
    )
    expect_equal(obj$cache$ellipsoid[[1]]$log_vol, 0.4515827)
    expect_equal(
      obj$cache$ellipsoid[[1]]$inv_sqrt_shape,
      diag(sqrt(2 / 4), nrow = 2)
    )
  })
})

test_that("multi_ellipsoid can provide good results", {
  skip_extended()
  expect_gaussian_run(multi_ellipsoid())
  expect_3D_run(multi_ellipsoid())
})
