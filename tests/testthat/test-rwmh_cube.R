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
  ptypes <- list(epsilon = double(), n_accept = integer())

  it("Can be built and propose points", {
    obj <- expect_all_proposals(
      new_rwmh_cube,
      unit_log_fn = fn,
      n_dim = 2,
      extra_args = "n_accept"
    )
    expect_lrps(obj, subclass = "rwmh_cube", !!!ptypes)
  })

  it("Can be updated with a matrix of points", {
    obj <- new_rwmh_cube(fn, n_dim = 2)
    samples <- run_sampler(obj)
    acc_ratio <- sum(samples$n_accept) / sum(samples$neval)
    new_eps <- exp((acc_ratio - 0.5) / 2 / 0.5)

    new_obj <- update_lrps(obj, samples$unit)
    expect_lrps(new_obj, subclass = "rwmh_cube", !!!ptypes)
    expect_equal(new_obj$cache$epsilon, new_eps)
    expect_identical(new_obj$cache$neval, 0L)
    expect_identical(new_obj$cache$n_accept, 0L)
    new_samples <- run_sampler(new_obj)

    skip_extended()
    f <- test_plot(samples$unit, new_samples$unit)
    vdiffr::expect_doppelganger("rwmh", f)
  })

  it("Can be updated without a matrix", {
    obj <- new_rwmh_cube(fn, n_dim = 2)
    samples <- run_sampler(obj)
    expect_idempotent_update(
      obj,
      "rwmh_cube",
      reset = "n_accept",
      ptypes = ptypes
    )
  })
})

test_that("rwmh_cube can provide good results", {
  skip_extended()
  expect_gaussian_run(rwmh_cube())
  expect_3D_run(rwmh_cube())
  expect_eggbox_run(rwmh_cube())
})
