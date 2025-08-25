sampler <- ernest_sampler(
  log_lik = gaussian_blobs$log_lik,
  prior = gaussian_blobs$prior
)
run1 <- generate(sampler, max_iterations = 100, seed = 42)

test_that("Throws errors when stop criteria are already passed", {
  cur_calls <- run1$n_call
  cur_minlogz <- run1$n_minlogz
  log_vol <- run1$log_volume[run1$n_iter]
  log_z <- run1$log_evidence[run1$n_iter]
  cur_minlogz <- logaddexp(0, max(run1$log_lik) + log_vol - log_z)

  expect_snapshot_error(generate(run1, max_iterations = 50))
  expect_snapshot_error(generate(run1, max_calls = cur_calls))
  expect_snapshot_error(generate(run1, min_logz = cur_minlogz + 0.1))
})

run2 <- generate(run1, max_iterations = 1000, seed = 42)
test_that("Resuming using an ernest_run", {
  expect_equal(run2$n_iter, 1000)
  expect_identical(run1$log_volume[1:100], run2$log_volume[1:100])
  expect_identical(run1$log_lik[1:100], run2$log_lik[1:100])
  expect_identical(run1$samples_unit[1:100, ], run2$samples_unit[1:100, ])

  skip_if(getOption("ernest.extended_tests", FALSE))
  run3 <- generate(run2, min_logz = 0.5)
  expect_identical(run3$log_volume[1:1000], run2$log_volume[1:1000])
  expect_identical(run3$log_lik[1:1000], run2$log_lik[1:1000])
  expect_identical(run3$samples_unit[1:1000, ], run2$samples_unit[1:1000, ])
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
