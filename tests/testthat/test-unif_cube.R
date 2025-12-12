fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

test_that("unif_cube returns correct class and structure", {
  default <- unif_cube()
  expect_equal(default$max_loop, 1e6L)
  expect_snapshot(default)
})

describe("unif_cube class", {
  it("Can be built and propose points", {
    obj <- expect_all_proposals(
      new_unif_cube,
      unit_log_fn = fn,
      n_dim = 2,
      allow_failure = TRUE
    )
    expect_lrps(obj, subclass = "unif_cube")
  })

  it("Can be updated with a matrix of points", {
    obj <- new_unif_cube(fn, n_dim = 2)
    samples <- run_sampler(obj)

    new_obj <- update_lrps(obj, samples$unit)
    expect_lrps(new_obj, subclass = "unif_cube")
    expect_identical(new_obj$cache$n_call, 0L)
    new_samples <- run_sampler(new_obj, samples$unit)

    skip_extended()
    f <- test_plot(samples$unit, new_samples$unit)
    vdiffr::expect_doppelganger("unif_cube", f)
  })

  it("Can be updated without a matrix", {
    obj <- new_unif_cube(fn, n_dim = 2)
    samples <- run_sampler(obj)
    expect_idempotent_update(obj, "unif_cube")
  })
})

test_that("unif_cube can provide good results", {
  skip_extended()
  expect_gaussian_run(unif_cube(), .generate = list(min_logz = 1))
})
