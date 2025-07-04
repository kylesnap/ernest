test_that("sim_volume returns expected", {
  sim_volume(500, 4000)
  mean_vol <- rowMeans(replicate(1000, sim_volume(500, 4000)))
  expect_equal(
    mean_vol,
    c(-seq(4000)/500, -(4000/500) - (digamma(501) - digamma(501 - seq(500)))),
    tolerance = 0.01
  )
  expect_equal(
    sum(!is.finite(mean_vol)),
    0
  )
})

test_that("get_logweight calculates correctly", {
  expected <- readRDS(test_path("./sample_run.rds"))
  log_w <- get_logweight(expected$log_lik, expected$log_volume)
  expect_equal(log_w, expected$log_weight, tolerance = 1e-6)
})

test_that("get_logevid calculates correctly", {
  expected <- readRDS(test_path("./sample_run.rds"))
  log_w <- get_logweight(expected$log_lik, expected$log_volume)
  log_z <- get_logevid(log_w)
  log_evid <- as.double(expected$log_evidence)
  expect_equal(log_z, log_evid)
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
  expect_equal(drop(posterior::draws_of(calc$log_evidence.err)), sqrt(run$log_evidence_var))
})

test_that("calculate works when ndraws = 1", {
  run <- readRDS(test_path("./example_run.rds"))
  n_samp <- run$n_iter + run$n_points

  calc <- calculate(run, ndraws = 1)
  expect_equal(drop(posterior::draws_of(calc$log_lik)), run$log_lik)
  expect_equal(dim(posterior::draws_of(calc$log_volume)), c(1, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_weight)), c(1, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_evidence)), c(1, n_samp))
})

test_that("calculate works when ndraws = BIG", {
  run <- readRDS(test_path("./example_run.rds"))
  n_samp <- run$n_iter + run$n_points

  calc <- calculate(run, ndraws = 1000)
  expect_equal(drop(posterior::draws_of(calc$log_lik)), run$log_lik)
  expect_equal(dim(posterior::draws_of(calc$log_volume)), c(1000, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_weight)), c(1000, n_samp))
  expect_equal(dim(posterior::draws_of(calc$log_evidence)), c(1000, n_samp))

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
})


