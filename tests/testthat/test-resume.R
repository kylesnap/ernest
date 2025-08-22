sampler <- ernest_sampler(
  log_lik = gaussian_blobs$log_lik,
  prior = gaussian_blobs$prior
)

test_that("Runs can be completed and resume", {
  run1 <- generate(sampler, max_iterations = 100, seed = 42)
  run2 <- generate(run1, max_iterations = 1000)

  expect_equal(run2$n_iter, 1000)
  expect_identical(run1$log_volume[1:100], run2$log_volume[1:100])
  expect_identical(run1$log_evidence[1:100], run2$log_evidence[1:100])
  expect_identical(run1$log_lik[1:100], run2$log_lik[1:100])
  expect_identical(run1$samples_unit[1:100, ], run2$samples_unit[1:100, ])

  run3 <- generate(run2, min_logz = 0.5)
  expect_identical(run3$log_volume[1:1000], run2$log_volume[1:1000])
  expect_identical(run3$log_evidence[1:1000], run2$log_evidence[1:1000])
  expect_identical(run3$log_lik[1:1000], run2$log_lik[1:1000])
  expect_identical(run3$samples_unit[1:1000, ], run2$samples_unit[1:1000, ])
  expect_snapshot_error(generate(run3, min_logz = 0.5))
})

test_that("Throws errors when stop criteria are already passed", {
  sampler <- ernest_sampler(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior
  )
  run1 <- generate(sampler, max_iterations = 100, seed = 42L)
  expect_snapshot_error(generate(run1, max_iterations = 50))

  run1 <- generate(sampler, max_calls = 1000, seed = 42L)
  expect_snapshot_error(generate(run1, max_calls = 500))
})
