set.seed(42)
fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()

test_that("unif_ellipsoid can be called by user", {
  default <- unif_ellipsoid()
  expect_snapshot(unif_ellipsoid(enlarge = 0.5), error = TRUE)
  expect_snapshot(default)
})

describe("unif_ellipsoid class", {
  ptypes <- list(
    "center" = double(),
    "shape" = vctrs::vec_ptype(matrix(double(), ncol = 2)),
    "inv_sqrt_shape" = vctrs::vec_ptype(matrix(double(), ncol = 2)),
    "log_volume" = double()
  )

  it("Can be built and propose points", {
    obj <- expect_all_proposals(
      new_unif_ellipsoid,
      unit_log_fn = fn,
      nvar = 2,
      allow_failure = TRUE
    )
    expect_lrps(obj, subclass = "unif_ellipsoid", !!!ptypes)
    expect_equal(obj$enlarge, 1)
    expect_equal(obj$cache$log_volume, 0.4515827)
    expect_equal(obj$cache$inv_sqrt_shape, diag(sqrt(2 / 4), nrow = 2))
    expect_equal(obj$cache$center, c(0.5, 0.5))
  })

  it("Can be updated with a matrix of points", {
    obj <- new_unif_ellipsoid(unit_log_fn = fn, nvar = 2)
    samples <- run_sampler(obj)
    expect_lrps(obj, subclass = "unif_ellipsoid", !!!ptypes)
    original_volume <- obj$cache$log_volume

    new_obj <- update_lrps(obj, samples$unit)
    expect_lrps(new_obj, subclass = "unif_ellipsoid", !!!ptypes)
    expect_lte(new_obj$cache$log_volume, original_volume)
    new_samples <- run_sampler(new_obj, samples$unit)

    skip_extended()
    f <- test_plot(samples$unit, new_samples$unit)
    vdiffr::expect_doppelganger("unif_ellipsoid", f)
  })

  it("Can be updated without a matrix", {
    obj <- new_unif_ellipsoid(unit_log_fn = fn, nvar = 2)
    samples <- run_sampler(obj)
    expect_idempotent_update(
      obj,
      "unif_ellipsoid",
      ptypes = ptypes
    )
  })

  it("Throws a warning when provided with constant points", {
    obj <- new_unif_ellipsoid(unit_log_fn = fn, nvar = 2)
    live <- matrix(rep(0.5, 500 * 2), nrow = 500)
    expect_warning(
      update_lrps(obj, live),
      "Ellipsoid fitting returned an error code"
    )
    expect_equal(obj$cache$log_volume, 0.4515827)
    expect_equal(obj$cache$inv_sqrt_shape, diag(sqrt(2 / 4), nrow = 2))
    expect_equal(obj$cache$center, c(0.5, 0.5))
  })
})

test_that("unif_ellipsoid can provide good results", {
  skip_extended()
  expect_gaussian_run(unif_ellipsoid())
})
