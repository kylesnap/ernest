test_that("sim_volume returns expected", {
  mean_vol <- rowMeans(replicate(1000, sim_volume(500, 4000)))
  expect_equal(
    mean_vol,
    c(-seq(4000)/500, -(4000/500) - (digamma(501) - digamma(501 - seq(500)))),
    tolerance = 0.01
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

# gaussian_2 <- make_gaussian(2)
# sampler <- nested_sampling(gaussian_2$log_lik, gaussian_2$prior)
# run <- generate(sampler, min_logz = 0.05)

test_that("calculate works when ndraws = 0", {
  run <- readRDS(test_path("./example_run.rds"))
  calc <- calculate(run, ndraws = 0)
  expect_equal(calc$log_lik, run$log_lik)
  expect_equal(calc$log_volume, run$log_volume)
  expect_equal(calc$log_weight, run$log_weight)
  expect_equal(calc$log_evidence, run$log_evidence)
  expect_equal(calc$information, run$information)
})

test_that("calculate works when ndraws = 1", {
  run <- readRDS(test_path("./example_run.rds"))
  expect_no_condition(calculate(run, ndraws = 1))
})

test_that("calculate works when ndraws = BIG", {
  run <- readRDS(test_path("./example_run.rds"))
  expect_no_condition(calculate(run, ndraws = 4000))
})
