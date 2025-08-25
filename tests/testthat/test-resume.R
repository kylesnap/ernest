sampler <- ernest_sampler(
  log_lik = gaussian_blobs$log_lik,
  prior = gaussian_blobs$prior
)
run1 <- generate(sampler, max_iterations = 100, seed = 42)
run2 <- generate(run1, max_iterations = 1000, seed = 42)

test_that("Resuming using an ernest_run", {
  expect_equal(run2$n_iter, 1000)
  expect_identical(run1$log_volume[1:100], run2$log_volume[1:100])
  expect_identical(run1$log_lik[1:100], run2$log_lik[1:100])
  expect_identical(run1$samples_unit[1:100, ], run2$samples_unit[1:100, ])

  skip_extended_test()
  run3 <- generate(run2, min_logz = 0.5, seed = 42)
  expect_identical(run3$log_volume[1:1000], run2$log_volume[1:1000])
  expect_identical(run3$log_lik[1:1000], run2$log_lik[1:1000])
  expect_identical(run3$samples_unit[1:1000, ], run2$samples_unit[1:1000, ])
})

test_that("Reset an existing run", {
  run1_restart <- generate(run1, max_iterations = 100, seed = 42, clear = TRUE)
  expect_identical(run1$log_volume[1:100], run1_restart$log_volume[1:100])
  expect_identical(run1$log_lik[1:100], run1_restart$log_lik[1:100])
  expect_identical(
    run1$samples_unit[1:100, ],
    run1_restart$samples_unit[1:100, ]
  )
})

test_that("Reproducing a ernest_sampler saved to disk", {
  withr::with_file("sampler.rds", {
    saveRDS(sampler, "sampler.rds")
    f_sampler <- readRDS("sampler.rds")
    f_run1 <- generate(f_sampler, max_iterations = 100, seed = 42)
    expect_identical(run1$log_volume, f_run1$log_volume)
    expect_identical(run1$log_lik, f_run1$log_lik)
    expect_identical(run1$samples_unit, f_run1$samples_unit)
  })
})
