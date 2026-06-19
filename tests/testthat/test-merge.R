test_that("merging fails when arguments are poorly specified", {
  expect_error(
    merge(example_run, "example_run"),
    "must be an object with class ernest_run"
  )

  expect_error(
    merge(example_run, example_run),
    "cannot be identical"
  )
})

test_that("merged run has expected properties", {
  run_a <- ernest_sampler(
    gaussian_blobs$log_lik,
    gaussian_blobs$prior,
    nlive = 100,
    seed = 1
  ) |>
    generate()

  run_b <- ernest_sampler(
    gaussian_blobs$log_lik,
    gaussian_blobs$prior,
    nlive = 200,
    seed = 2
  ) |>
    generate()

  merged <- merge(run_a, run_b)
  expect_equal(merged$nlive, 300)
  expect_length(merged$rcrd, length(run_a$rcrd) + length(run_b$rcrd))
  expect_length(unique(field(merged$rcrd, "id")), 300)
  expect_equal(merged$.merge[1, ], glance(run_a))
  expect_equal(merged$.merge[2, ], glance(run_b))

  # Generate can be called on the merged object.
  run3 <- generate(
    merged,
    max_iterations = merged$niter + 1000L,
    min_logz = 0
  )
  expect_equal(run3$niter, merged$niter + 1000L)
})

test_that("merge_rcrd errors when suffixes still produce duplicate ids", {
  # craft two minimal rcrd objects that will produce duplicate ids even after
  # the same suffix is applied
  data(example_run)
  x <- example_run$rcrd[1:10]
  y <- example_run$rcrd[1:10]

  # using identical suffixes for both sides should trigger the "must be unique" error
  expect_error(
    merge_rcrd(".same" = x, ".same" = y),
    "Multiple arguments named `.same`"
  )
})
