describe("merge_sampler", {
  x <- ernest_sampler(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior
  )

  it("catches errors", {
    expect_error(
      merge_sampler(x, list()),
      "must be an object with class ernest_sampler"
    )
    expect_error(
      merge_sampler(1, x),
      "must be an object with class ernest_sampler"
    )
    sampler_3d <- ernest_sampler(
      log_lik = gaussian_3D$log_lik,
      prior = gaussian_3D$prior
    )
    expect_error(
      merge_sampler(x, sampler_3d),
      "must have the same prior variable names"
    )
    sampler_alt <- ernest_sampler(
      log_lik = gaussian_blobs$log_lik,
      prior = gaussian_blobs$prior,
      sampler = unif_cube()
    )
    expect_error(
      merge_sampler(x, sampler_alt),
      "must have the same LRPS method"
    )
  })

  it("combines compatible samplers", {
    y <- ernest_sampler(
      log_lik = gaussian_blobs$log_lik,
      prior = gaussian_blobs$prior
    )

    z <- merge_sampler(x, y)
    expect_identical(z$nlive, 1000L)
    expect_identical(z$first_update, 1250L)
    expect_identical(z$update_interval, 750L)
  })

  it("uses default update values when they differ", {
    x <- ernest_sampler(
      log_lik = gaussian_blobs$log_lik,
      prior = gaussian_blobs$prior,
      first_update = 100,
      update_interval = 50,
      seed = 24
    )
    y <- ernest_sampler(
      log_lik = gaussian_blobs$log_lik,
      prior = gaussian_blobs$prior,
      first_update = 150,
      update_interval = 75,
      seed = 42
    )

    z <- merge_sampler(x, y)
    expect_identical(z$first_update, 2500L)
    expect_identical(z$update_interval, 1500L)
    expect_identical(attr(z, "seed"), NA_integer_)
  })
})

test_that("merging two runs", {
  run1 <- generate(
    ernest_sampler(
      log_lik = gaussian_blobs$log_lik,
      prior = gaussian_blobs$prior,
      seed = 24
    ),
    max_iterations = 100
  )
  run2 <- generate(
    ernest_sampler(
      log_lik = gaussian_blobs$log_lik,
      prior = gaussian_blobs$prior,
      nlive = 300,
      seed = 42
    ),
    max_iterations = 100
  )
  run3 <- merge(run1, run2)
  expect_s3_class(run3, c("ernest_run", "ernest_sampler"))
  expect_identical(run3$nlive, 800L)
  expect_identical(sort(unique(run3$weights$id)), seq(800L))

  run4 <- generate(run3, max_iterations = run3$niter + 100L, min_logz = 0)
  expect_gt(run4$niter, run3$niter)
  expect_s3_class(run4, c("ernest_run", "ernest_sampler"))
})
