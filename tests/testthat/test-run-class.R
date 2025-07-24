data(example_run)

test_that("example run returns as expected", {
  expect_s3_class(example_run, "ernest_run")
  expect_snapshot_value(example_run$id, style = "json2")
  expect_snapshot_value(example_run$points, style = "json2")
  expect_snapshot_value(example_run$birth, style = "json2")

  log_lik <- example_run$spec$log_lik
  prior_fn <- example_run$spec$prior$fn
  row_match <- matrix(
    nrow = example_run$n_points + example_run$n_iter,
    ncol = 3L
  )
  logl_match <- double(example_run$n_points + example_run$n_iter)
  for (i in seq_len(nrow(example_run$samples))) {
    row_match[i, ] <- prior_fn(example_run$samples_unit[i, ])
    logl_match[i] <- log_lik(row_match[i, ])
  }
  colnames(row_match) <- example_run$spec$prior$varnames
  expect_equal(row_match, example_run$samples)
  expect_equal(logl_match, example_run$log_lik)
  expect_equal(
    sort(attr(example_run, "live_loc")),
    seq(example_run$n_iter + 1, example_run$n_points + example_run$n_iter)
  )
  expect_snapshot(example_run)
})

test_that("Summary method expectation", {
  smry <- summary(example_run)
  expect_snapshot_value(smry$n_iter)
  expect_equal(smry$n_points, 500)
  expect_snapshot_value(smry$n_calls)
  expect_equal(smry$log_volume, tail(example_run$log_volume, 1))
  expect_equal(smry$log_evidence, tail(example_run$log_evidence, 1))

  expect_snapshot_value(smry$run, style = "serialize")
})
