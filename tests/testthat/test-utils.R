test_that("validate_integer_parameter works correctly", {
  expect_equal(round_to_integer(5L, 10), 5L)
  expect_equal(round_to_integer(5.5, 10), 55L)

  expect_equal(round_to_integer(0.5, 10), 5L)
  expect_error(round_to_integer("a", 10), "must be a number")
})

test_that("check_unique_names detects duplicates", {
  expect_error(check_unique_names(c("a", "b", "a")), "duplicated: a")
  expect_silent(check_unique_names(c("a", "b", "c")))
})

test_that("create_live generates live points correctly", {
  lrps <- rwcube_lrps$new(
    log_lik_fn = gaussian_2$log_lik,
    prior_fn = compile(ernest_prior(gaussian_2$prior_transform)),
    n_dim = 2L
  )
  result <- create_live(lrps, 10, 2)
  expect_equal(dim(result$unit), c(10, 2))
  expect_equal(dim(result$point), c(10, 2))
  expect_length(result$log_lik, 10)
})

test_that("check_live validates live points correctly", {
  # Test case: Valid live points
  live <- list(
    unit = matrix(runif(20, 0, 1), nrow = 5, ncol = 4),
    point = matrix(rnorm(20), nrow = 5, ncol = 4),
    log_lik = c(-10, -5, -3, -1, -0.5)
  )
  expect_silent(check_live(live, n_points = 5, n_var = 4))

  # Test case: Empty live points
  live <- list()
  expect_error(
    check_live(live, n_points = 5, n_var = 4),
    "No live points found."
  )

  # Test case: live$unit is not a matrix
  live <- list(
    unit = runif(20),
    point = matrix(rnorm(20), nrow = 5, ncol = 4),
    log_lik = c(-10, -5, -3, -1, -0.5)
  )
  expect_error(
    check_live(live, n_points = 5, n_var = 4),
    "Unit points must be stored as a matrix."
  )

  # Test case: live$unit dimensions do not match
  live <- list(
    unit = matrix(runif(15, 0, 1), nrow = 3, ncol = 5),
    point = matrix(rnorm(20), nrow = 5, ncol = 4),
    log_lik = c(-10, -5, -3, -1, -0.5)
  )
  expect_error(
    check_live(live, n_points = 5, n_var = 4),
    "Unit points must be stored as a matrix with dim"
  )

  # Test case: live$unit contains non-finite values
  live <- list(
    unit = matrix(c(runif(19, 0, 1), Inf), nrow = 5, ncol = 4),
    point = matrix(rnorm(20), nrow = 5, ncol = 4),
    log_lik = c(-10, -5, -3, -1, -0.5)
  )
  expect_error(
    check_live(live, n_points = 5, n_var = 4),
    "Unit points must contain only finite values."
  )

  # Test case: live$unit contains values outside [0, 1]
  live <- list(
    unit = matrix(c(runif(19, 0, 1), 1.5), nrow = 5, ncol = 4),
    point = matrix(rnorm(20), nrow = 5, ncol = 4),
    log_lik = c(-10, -5, -3, -1, -0.5)
  )
  expect_error(
    check_live(live, n_points = 5, n_var = 4),
    "Unit points must contain values within \\[0, 1\\]."
  )

  # Test case: live$point is not a matrix
  live <- list(
    unit = matrix(runif(20, 0, 1), nrow = 5, ncol = 4),
    point = runif(20),
    log_lik = c(-10, -5, -3, -1, -0.5)
  )
  expect_error(
    check_live(live, n_points = 5, n_var = 4),
    "Live points couldn't be stored as a matrix."
  )

  # Test case: live$log_lik contains non-finite values
  live <- list(
    unit = matrix(runif(20, 0, 1), nrow = 5, ncol = 4),
    point = matrix(rnorm(20), nrow = 5, ncol = 4),
    log_lik = c(-10, -5, -3, -1, Inf)
  )
  expect_error(
    check_live(live, n_points = 5, n_var = 4),
    "Log-likelihood values can only be finite or `-Inf`."
  )

  # Test case: live$log_lik contains only one unique value
  live <- list(
    unit = matrix(runif(20, 0, 1), nrow = 5, ncol = 4),
    point = matrix(rnorm(20), nrow = 5, ncol = 4),
    log_lik = rep(-10, 5)
  )
  expect_error(
    check_live(live, n_points = 5, n_var = 4),
    "Every point had a calculated log-lik. value of -10."
  )
})

test_that("which_minn works correctly", {
  x <- c(10, 20, 5, 15)
  expect_equal(which_minn(x, 1), 3)
  expect_equal(which_minn(x, 2), c(3, 1))
})

test_that("compute_integral delivers expected results", {
  expected <- readRDS(test_path("./compute_integral_test.rds"))
  observed <- compute_integral(expected$log_lik, expected$log_vol)

  expect_equal(observed$log_weight, expected$log_wt)
  expect_equal(observed$log_evidence, expected$log_z)
  expect_equal(observed$log_evidence.var, expected$log_z_var)
  expect_equal(observed$information, expected$h)
})
