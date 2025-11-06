sampler <- ernest_sampler(
  log_lik = gaussian_blobs$log_lik,
  prior = gaussian_blobs$prior,
  seed = 42
)
run1 <- generate(sampler, max_iterations = 100)

test_that("Reproducing a ernest_sampler saved to disk", {
  withr::local_file("sampler.rds")
  saveRDS(sampler, "sampler.rds")
  f_sampler <- readRDS("sampler.rds")
  f_run1 <- generate(f_sampler, max_iterations = 100)
  expect_identical(run1$log_volume, f_run1$log_volume)
  expect_identical(run1$log_lik, f_run1$log_lik)
  expect_identical(run1$samples_unit, f_run1$samples_unit)
})
