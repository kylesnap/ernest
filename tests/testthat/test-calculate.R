set.seed(42L)

test_that("sim_volume can draw with ndraws = 1", {
  mat <- sim_volume(500, 5000, ndraws = 1)
  expect_equal(dim(mat), c(1, 5500))
  expect_true(all(mat < 0))
})

test_that("sim_volume can draw expected values", {
  mat <- sim_volume(500, 4000, ndraws = 4000)
  expect_equal(dim(mat), c(4000, 4500))

  mean_vols <- colMeans(mat)
  sd_vols <- apply(mat, 2, sd)

  expected_means <- c(
    # For dead points: E(Vol[i]) = Sum of log of kth-Uniform order stat.
    # or: E(ln Vol[i]) = E(Beta(K, 1))
    -seq(4000) / 500,
    # For live: E(Vol[i]) = Uniform order statistics from 1 -> K,
    # relative to the last volume.
    # Or: E(Beta(K, 1)) - E(Beta(K + 1, n + 1 - i))
    -(4000 / 500) - (digamma(501) - digamma(501 - seq(500)))
  )

  expect_true(all(is.finite(sd_vols)))
  expect_true(all(abs(mean_vols - expected_means) < sd_vols))
  expect_equal(mean_vols, expected_means, tolerance = 0.1)
})

test_that("get_logweight calculates correctly", {
  expected <- readRDS(test_path("./sample_run.rds"))
  log_w <- get_logweight(expected$log_lik, expected$log_volume)
  expected_w <- as.numeric(expected$log_weight)
  expect_equal(log_w, expected_w)

  mat_vol <- matrix(rep(expected$log_volume, 50), byrow = TRUE, nrow = 50)
  mat_expected <- matrix(rep(expected_w, 50), byrow = TRUE, nrow = 50)
  expect_equal(
    mat_expected,
    get_logweight(expected$log_lik, mat_vol)
  )
})

test_that("get_logevid calculates correctly", {
  expected <- readRDS(test_path("./sample_run.rds"))
  log_w <- get_logweight(expected$log_lik, expected$log_volume)
  expected_evid <- as.double(expected$log_evidence)
  log_z <- get_logevid(log_w)
  expect_equal(log_z, expected_evid)

  mat_vol <- matrix(rep(expected$log_volume, 50), byrow = TRUE, nrow = 50)
  mat_expected_evid <- matrix(rep(expected_evid, 50), byrow = TRUE, nrow = 50)
  expect_equal(
    get_logevid(get_logweight(expected$log_lik, mat_vol)),
    mat_expected_evid
  )
})

test_that("get_information calculates correctly", {
  expected <- readRDS(test_path("./sample_run.rds"))
  log_w <- get_logweight(expected$log_lik, expected$log_volume)
  log_z <- get_logevid(log_w)
  h <- get_information(expected$log_lik, expected$log_volume, log_z)
  expected_info <- as.double(expected$information)
  expect_equal(h, expected_info)
})

test_that("calculate works when ndraws = 0", {
  run <- readRDS(test_path("./example_run.rds"))
  calc <- calculate(run, ndraws = 0)
  expect_equal(drop(posterior::draws_of(calc$log_lik)), run$log_lik)
  expect_equal(drop(posterior::draws_of(calc$log_volume)), run$log_volume)
  expect_equal(drop(posterior::draws_of(calc$log_weight)), run$log_weight)
  expect_equal(drop(posterior::draws_of(calc$log_evidence)), run$log_evidence)
  expect_equal(
    drop(posterior::draws_of(calc$log_evidence_err)),
    sqrt(run$log_evidence_var)
  )

  smry <- summary(calc)
  expect_equal(smry$n_draws, 0)
  expect_equal(smry$log_evidence, run$log_evidence[length(run$log_evidence)])
  expect_equal(
    smry$log_evidence_err,
    sqrt(run$log_evidence_var[length(run$log_evidence_var)])
  )
  expect_true(inherits(smry, "summary.ernest_estimate"))

  expect_snapshot(calc)
  expect_snapshot(smry)
})

test_that("calculate works when ndraws = 1", {
  run <- readRDS(test_path("./example_run.rds"))
  n_samp <- run$n_iter + run$n_points
  calc <- calculate(run, ndraws = 1)
  expect_equal(drop(posterior::draws_of(calc$log_lik)), run$log_lik)
  expect_equal(dim(posterior::draws_of(calc$log_volume)), c(1, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_weight)), c(1, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_evidence)), c(1, n_samp))

  smry <- summary(calc)
  expect_equal(smry$n_draws, 1)
  expect_equal(smry$log_evidence_err, Inf)
  expect_true(inherits(smry, "summary.ernest_estimate"))

  expect_snapshot(calc)
  expect_snapshot(smry)
})

test_that("calculate works when ndraws = 4000 (default)", {
  run <- readRDS(test_path("./example_run.rds"))
  n_samp <- run$n_iter + run$n_points

  calc <- calculate(run)
  expect_equal(drop(posterior::draws_of(calc$log_lik)), run$log_lik)
  expect_equal(dim(posterior::draws_of(calc$log_volume)), c(4000, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_weight)), c(4000, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_evidence)), c(4000, n_samp))

  expect_equal(
    (mean(calc$log_volume) - run$log_volume) <
      .Machine$double.eps + 2 * posterior::sd(calc$log_volume),
    rep(TRUE, n_samp)
  )
  expect_equal(
    (mean(calc$log_weight) - run$log_weight) <
      .Machine$double.eps + 2 * posterior::sd(calc$log_weight),
    rep(TRUE, n_samp)
  )
  expect_equal(
    (mean(calc$log_evidence) - run$log_evidence) <
      .Machine$double.eps + 2 * posterior::sd(calc$log_evidence),
    rep(TRUE, n_samp)
  )

  smry <- summary(calc)
  expect_equal(smry$n_draws, 4000L)
  expect_true(
    abs(smry$log_evidence - run$log_evidence[length(run$log_evidence)]) <
      2 * smry$log_evidence_err
  )
  expect_true(smry$log_evidence_err > 0)
  expect_true(inherits(smry, "summary.ernest_estimate"))

  expect_snapshot(calc)
  expect_snapshot(smry)
})
