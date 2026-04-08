withr::local_seed(42)

#' Testing calculate against values produced by `nestcheck` when provided
#' a sample run from PolyChord.
gold <- readRDS(test_path("calculate-gold.rds"))
rcrd <- new_ernest_rcrd(
  unit = matrix(0, nrow = length(gold$log_lik), ncol = 2),
  log_lik = gold$log_lik,
  id = c(
    rep(seq(250), length.out = length(gold$log_lik) - 250),
    rev(seq(250))
  ),
  nlive = get_points(gold$log_lik, 250, TRUE),
  evals = c(
    rep(1, length.out = length(gold$log_lik) - 250),
    rep(0, 250)
  ),
  birth_lik = rep(-Inf, length.out = length(gold$log_lik))
)

test_that("Helpers produce as expected", {
  expect_equal(
    get_points(c(10, 9, 8, 7, 6, 5, 4), 3, TRUE),
    c(3, 3, 3, 3, 3, 2, 1)
  )
  expect_equal(
    get_points(c(10, 10, 9, 8, 7, 7, 6), 3, TRUE),
    c(3, 2, 3, 3, 3, 2, 1)
  )
  expect_equal(
    get_points(c(10, 10, 9, 8, 7, 7, 6), 3, FALSE),
    c(3, 2, 3, 3, 3, 2, 3)
  )

  expect_equal(drop(get_log_vol(rcrd)), gold$log_volume)
  calc <- get_log_w(gold$log_lik, gold$log_volume)
  expect_equal(drop(calc$log_weight), gold$log_weight)
  expect_equal(drop(calc$log_evidence), gold$log_evidence)

  expect_warning(get_log_vol(rev(rcrd)), "'log_lik')` is not a sorted vector.")
})

test_that("Simulated log vols do not diverge from mean estimates", {
  set.seed(42)
  log_vol <- get_log_vol(rcrd, ndraws = 4000)

  expect_equal(
    abs(colMeans(log_vol) - gold$log_volume) < matrixStats::colSds(log_vol),
    rep(TRUE, 3000)
  )
})

test_that("calculate works when ndraws = 0", {
  data(example_run)
  calc <- calculate(example_run, ndraws = 0)
  expect_equal(calc$log_lik, example_run$weights$log_lik)
  expect_equal(calc$log_weight, example_run$weights$log_weight)
  expect_equal(tail(calc$log_evidence, 1), example_run$log_evidence)
  expect_equal(tail(calc$log_evidence_err, 1), example_run$log_evidence_err)
  expect_snapshot(calc)
})

test_that("calculate works when ndraws = 1", {
  data(example_run)
  n_samp <- example_run$niter + example_run$nlive
  calc <- calculate(example_run, ndraws = 1)
  expect_equal(calc$log_lik, example_run$weights$log_lik)
  expect_equal(dim(posterior::draws_of(calc$log_volume)), c(1, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_weight)), c(1, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_evidence)), c(1, n_samp))

  expect_snapshot(calc)
})

test_that("calculate works when ndraws = 1000 (default)", {
  skip_extended()
  data(example_run)
  n_samp <- example_run$niter + example_run$nlive

  calc <- calculate(example_run)
  expect_equal(calc$log_lik, example_run$weights$log_lik)
  expect_equal(dim(posterior::draws_of(calc$log_volume)), c(1000, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_weight)), c(1000, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_evidence)), c(1000, n_samp))

  log_z <- tail(calc$log_evidence, 1)
  expect_lt(
    abs(mean(log_z) - example_run$log_evidence),
    .Machine$double.eps + 3 * posterior::sd(log_z)
  )

  expect_snapshot(calc)
})
