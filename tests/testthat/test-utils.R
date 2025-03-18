test_that("validate_integer_parameter works correctly", {
  expect_equal(validate_integer_parameter(5L, 10), 5L)
  expect_equal(validate_integer_parameter(5.5, 10), 55L)

  expect_equal(validate_integer_parameter(0.5, 10), 5L)
  expect_error(validate_integer_parameter("a", 10), "must be a number")
  expect_error(
    validate_integer_parameter(5.5, 10, min = 60),
    "larger than or equal to 60"
  )
})

test_that("Errors are thrown for compute_integral", {
  log_lik <- c(1, 2, 3)
  log_vol <- c(-0.5, -0.75)
  expect_error(
    compute_integral(log_lik, log_vol),
    "must have the same length"
  )
})

test_that("compute_integral delivers expected results", {
  expected <- readRDS(test_path("./compute_integral_test.rds"))
  observed <- compute_integral(expected$log_lik, expected$log_vol)

  expect_equal(expected$log_wt, observed$log_weight)
  expect_equal(expected$log_z, observed$log_z)
  expect_equal(expected$log_z_var, observed$log_z_var)
  expect_equal(expected$h, observed$information)
})
