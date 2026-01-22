skip("TEMPORARY")
data(example_run)

test_that("ernest_run object structure and content", {
  expect_s3_class(example_run, "ernest_run")
  expect_true(is.list(example_run))
  expect_true(all(
    c("id", "points", "birth_lik", "samples", "samples_unit", "log_lik") %in%
      names(example_run)
  ))
  expect_type(example_run$id, "integer")
  expect_type(example_run$points, "integer")
  expect_type(example_run$birth_lik, "double")
  expect_type(example_run$samples, "double")
  expect_type(example_run$samples_unit, "double")
  expect_type(example_run$log_lik, "double")
  expect_equal(length(example_run$id), length(example_run$log_lik))
  expect_equal(dim(example_run$samples_unit)[1], length(example_run$log_lik))
  expect_equal(dim(example_run$samples)[1], length(example_run$log_lik))
  expect_snapshot(example_run)
})

test_that("ernest_run log_lik and prior mapping", {
  log_lik <- example_run$log_lik_fn
  prior_fn <- example_run$prior$fn
  n <- example_run$n_points + example_run$n_iter
  row_match <- matrix(nrow = n, ncol = 3L)
  logl_match <- double(n)
  for (i in seq_len(nrow(example_run$samples))) {
    row_match[i, ] <- prior_fn(example_run$samples_unit[i, ])
    logl_match[i] <- log_lik(row_match[i, ])
  }
  colnames(row_match) <- example_run$spec$prior$names
  dimnames(example_run$samples) <- NULL
  expect_equal(row_match, example_run$samples)
  expect_equal(logl_match, example_run$log_lik)
})

test_that("summary.ernest_run returns expected structure and values", {
  smry <- summary(example_run)
  expect_s3_class(smry, "summary.ernest_run")
  expect_true(is.list(smry))
  expect_true(all(
    c(
      "n_iter",
      "n_points",
      "neval",
      "log_volume",
      "log_evidence",
      "log_evidence_err",
      "run",
      "draws"
    ) %in%
      names(smry)
  ))
  expect_equal(smry$n_points, example_run$n_points)
  expect_equal(smry$log_volume, tail(example_run$log_volume, 1))
  expect_equal(smry$log_evidence, tail(example_run$log_evidence, 1))
  expect_type(smry$n_iter, "integer")
  expect_type(smry$neval, "integer")
  expect_type(smry$log_volume, "double")
  expect_type(smry$log_evidence, "double")
  expect_type(smry$log_evidence_err, "double")
  expect_true(is.data.frame(smry$run))
  expect_true(nrow(smry$run) >= 1)
})

test_that("summary.ernest_run run tibble columns", {
  smry <- summary(example_run)
  expect_true(all(
    c(
      "call",
      "log_lik",
      "log_volume",
      "log_weight",
      "log_evidence",
      "log_evidence_err",
      "information"
    ) %in%
      names(smry$run)
  ))
  expect_equal(nrow(smry$run), example_run$n_iter + example_run$n_points)
  expect_type(smry$run$call, "integer")
  expect_type(smry$run$log_lik, "double")
  expect_type(smry$run$log_volume, "double")
  expect_type(smry$run$log_weight, "double")
  expect_type(smry$run$log_evidence, "double")
  expect_type(smry$run$log_evidence_err, "double")
  expect_type(smry$run$information, "double")
  expect_snapshot(smry)
})
