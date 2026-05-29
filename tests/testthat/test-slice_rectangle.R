fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("slice can be called by user", {
  default <- slice_rectangle()
  expect_snapshot(slice_rectangle(steps = 0), error = TRUE)
  expect_snapshot(slice_rectangle(adaptive = TRUE, max_steps = 2), error = TRUE)
  expect_equal(default$steps, 3L)
  expect_snapshot(default)
})

describe("slice class", {
  it("Can be built and propose points", {
    obj <- expect_all_proposals(
      new_slice_rectangle,
      unit_log_fn = fn,
      nvar = 2
    )
    expect_lrps(obj, subclass = "slice_rectangle")
    expect_snapshot(obj)
  })

  it("Can be updated with a matrix of points", {
    obj <- new_slice_rectangle(fn, nvar = 2)
    samples <- run_sampler(obj)
    new_obj <- update_lrps(obj, samples$unit)

    expect_true(
      inherits(new_obj, "slice_rectangle") || inherits(new_obj, "rwmh_cube")
    )
    new_samples <- run_sampler(new_obj)

    skip_extended()
    f <- test_plot(samples$unit, new_samples$unit)
    vdiffr::expect_doppelganger("slice_rectangle", f)
  })

  it("Can be updated without a matrix", {
    obj <- new_slice_rectangle(fn, nvar = 2)
    samples <- run_sampler(obj)
    expect_idempotent_update(
      obj,
      "slice_rectangle",
      reset = "n_accept",
    )
  })
})

test_that("slice_rectangle can provide good results", {
  skip_extended()
  expect_gaussian_run(slice_rectangle())
  expect_3D_run(slice_rectangle())
  expect_eggbox_run(slice_rectangle())
})

describe("adaptive slice", {
  obj <- new_slice_rectangle(
    unit_log_fn = fn,
    nvar = 2,
    adaptive = TRUE,
    steps = 2
  )
  obj$cache$whitening <- diag(2)
  obj$cache$mean_dist <- 0.52
  original <- c(0, 0)

  it("returns distances", {
    # Provide minimal whitening/mean_dist so adaptive branch can run
    res <- propose.slice_rectangle(obj, original = original, criterion = -Inf)
    expect_true(!is.null(res$distance))
    expect_type(res$distance, "double")
  })

  unit <- matrix(rnorm(200), ncol = 2)
  for (i in seq(100)) {
    unit[i, ] <- propose.slice_rectangle(
      obj,
      original = unit[i, ],
      criterion = -Inf
    )$unit
  }

  it("can be updated", {
    # Provide a matrix so the adaptive update branch can run
    new_obj <- update_lrps(obj, unit = unit)

    expect_s3_class(new_obj, "slice_rectangle")
    # If we got back a slice_rectangle, ensure steps were adjusted within bounds
    expect_true(new_obj$steps >= 1L && new_obj$steps <= new_obj$max_steps)
    expect_true(is.numeric(new_obj$cache$mean_dist))
  })
})
