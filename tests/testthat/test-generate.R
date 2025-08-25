sampler <- ernest_sampler(
  log_lik = gaussian_blobs$log_lik,
  prior = gaussian_blobs$prior
)
run1 <- generate(sampler, max_iterations = 100, seed = 42)

test_that("generate throws errors when stop criteria are already passed", {
  cur_calls <- run1$n_call
  log_vol <- run1$log_volume[run1$n_iter]
  log_z <- run1$log_evidence[run1$n_iter]
  cur_minlogz <- logaddexp(0, max(run1$log_lik) + log_vol - log_z)

  expect_snapshot_error(generate(run1, max_iterations = 50, seed = 42))
  expect_snapshot_error(generate(run1, max_calls = cur_calls, seed = 42))
  expect_snapshot_error(generate(run1, min_logz = cur_minlogz + 0.1, seed = 42))
})
