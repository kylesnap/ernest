withr::local_seed(42)

#' Testing calculate against values produced by `nestcheck` when provided
#' a sample run from PolyChord.
gold <- readRDS(test_path("calculate-gold.rds"))

test_that("Helpers produce as expected", {
  expect_equal(drop(get_logvol(250, 2750)), gold$log_volume)
  expect_equal(
    drop(get_logweight(gold$log_lik, matrix(gold$log_volume, nrow = 1))),
    gold$log_weight
  )
  expect_equal(
    drop(get_logevid(matrix(gold$log_weight, nrow = 1))),
    gold$log_evidence
  )
})

test_that("Simulated log vols do not diverge from mean estimates", {
  set.seed(42)
  log_vol <- get_logvol(250, 2750, ndraws = 4000)

  expect_equal(
    abs(colMeans(log_vol) - gold$log_volume) < matrixStats::colSds(log_vol),
    rep(TRUE, 3000)
  )
})

test_that("calculate works when ndraws = 0", {
  data(example_run)
  calc <- calculate(example_run, ndraws = 0)
  expect_equal(drop(posterior::draws_of(calc$log_lik)), example_run$log_lik)
  expect_equal(
    drop(posterior::draws_of(calc$log_volume)),
    example_run$log_volume
  )
  expect_equal(
    drop(posterior::draws_of(calc$log_weight)),
    example_run$log_weight
  )
  expect_equal(
    drop(posterior::draws_of(calc$log_evidence)),
    example_run$log_evidence
  )
  expect_equal(
    drop(posterior::draws_of(calc$log_evidence_err)),
    sqrt(example_run$log_evidence_var)
  )

  expect_snapshot(calc)
})

test_that("calculate works when ndraws = 1", {
  data(example_run)
  n_samp <- example_run$n_iter + example_run$n_points
  calc <- calculate(example_run, ndraws = 1)
  expect_equal(drop(posterior::draws_of(calc$log_lik)), example_run$log_lik)
  expect_equal(dim(posterior::draws_of(calc$log_volume)), c(1, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_weight)), c(1, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_evidence)), c(1, n_samp))

  expect_snapshot(calc)
})

test_that("calculate works when ndraws = 4000 (default)", {
  skip_extended_test()
  data(example_run)
  n_samp <- example_run$n_iter + example_run$n_points

  calc <- calculate(example_run)
  expect_equal(drop(posterior::draws_of(calc$log_lik)), example_run$log_lik)
  expect_equal(dim(posterior::draws_of(calc$log_volume)), c(4000, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_weight)), c(4000, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_evidence)), c(4000, n_samp))

  expect_equal(
    abs(mean(calc$log_evidence) - example_run$log_evidence) <
      .Machine$double.eps + 3 * posterior::sd(calc$log_evidence),
    rep(TRUE, n_samp)
  )

  expect_snapshot(calc)
})
