lrps <- new_rwmh_cube(
  log_lik = gaussian_2$log_lik,
  prior_transform = gaussian_2$prior_transform,
  n_dim = 2,
  update_interval = 100,
  num_steps = 20,
  target_acceptance = 0.25,
  init_epsilon <- 1
)
integral <- readRDS(test_path("./compute_integral_test.rds"))

test_that("calculate works correctly", {
  sampler <- ernest_sampler$new(lrps, 3, n_points = 100, verbose = FALSE)

  # Mocking private fields
  set.seed(500)
  dead_units <- matrix(runif(900 * 3), ncol = 3)
  dead_points <- matrix(runif(900 * 3), ncol = 3)
  sampler$.__enclos_env__$private$.dead <- list(
    unit = dead_units,
    point = dead_points,
    log_lik = integral$log_lik[1:900]
  )
  live_units <- matrix(runif(100 * 3), ncol = 3)
  live_points <- matrix(runif(100 * 3), ncol = 3)
  sampler$.__enclos_env__$private$.live <- list(
    unit = live_units,
    point = live_points,
    log_lik = integral$log_lik[901:1000]
  )
  sampler$.__enclos_env__$private$.log_vol <- integral$log_vol[1:900]
  sampler$.__enclos_env__$private$.progress <- vctrs::df_list(
    ".calls" = seq.int(0, by = 20, length.out = 900),
    ".id" = rep(1:500, length.out = 900),
    ".sampler" = rep(1, length.out = 900)
  )

  result <- sampler$calculate()
  expect_s3_class(result, "tbl_df")
  expect_mapequal(
    result, # Object
    tibble( # Expected
      ".iter" = 1:1000,
      log_lik = integral$log_lik,
      log_vol = integral$log_vol,
      log_z = integral$log_z,
      log_z_var = integral$log_z_var,
      log_weight = integral$log_wt,
      information = integral$h
    )
  )

  expect_equal(
    sampler$calculate(add_progress = TRUE)$.calls,
    c(seq.int(0, by = 20, length.out = 900), rep(NA, 100))
  )
  expect_equal(
    sampler$calculate(add_progress = TRUE)$.id,
    c(rep(1:500, length.out = 900), 1:100)
  )
  expect_equal(
    sampler$calculate(add_progress = TRUE)$.sampler,
    c(rep(1, length.out = 900), rep(NA, 100))
  )

  expect_equal(
    sampler$calculate(add_points = "unit") |> names(),
    c(".iter", "unit_X...1", "unit_X...2", "unit_X...3", "log_lik", "log_vol",
      "log_weight", "log_z", "log_z_var", "information")
  )
  expect_equal(
    sampler$calculate(add_points = "parameter") |> names(),
    c(".iter", "X...1", "X...2", "X...3", "log_lik", "log_vol",
      "log_weight", "log_z", "log_z_var", "information")
  )
})

test_that("calculate handles no iterations", {
  sampler <- ernest_sampler$new(lrps, 3, n_points = 100, verbose = FALSE)
  expect_message(sampler$calculate(), "No iterations have been performed")
})

test_that("calculate handles unsorted log_lik", {
  sampler <- ernest_sampler$new(lrps, ptype = 3, n_points = 10, verbose = FALSE)

  sampler$.__enclos_env__$private$.dead <- list(log_lik = runif(10))
  sampler$.__enclos_env__$private$.log_vol <- runif(10)
  sampler$.__enclos_env__$private$.live <- list(log_lik = sort(runif(10), decreasing = TRUE))
  sampler$.__enclos_env__$private$.n_points <- 10

  expect_snapshot_warning(
    res <- sampler$calculate()
  )
  expect_equal(
    names(res),
    c(".iter", "log_lik", "log_vol")
  )
})

test_that("calculate handles unsorted log_vol", {
  sampler <- ernest_sampler$new(lrps, ptype = 3, n_points = 10, verbose = FALSE)

  sampler$.__enclos_env__$private$.dead <- list(log_lik = runif(10))
  sampler$.__enclos_env__$private$.log_vol <- sort(runif(10), decreasing = TRUE)
  sampler$.__enclos_env__$private$.live <- list(log_lik = integral$log_lik[1:10])
  sampler$.__enclos_env__$private$.n_points <- 10

  expect_snapshot_warning(res <- sampler$calculate())
  expect_equal(
    names(res),
    c(".iter", "log_lik", "log_vol")
  )
})
