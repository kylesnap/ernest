gaussian_2 <- make_gaussian(2)
test_that("create_live generates live points correctly", {
  lrps <- rwcube_lrps$new(
    log_lik_fn = gaussian_2$log_lik,
    prior_fn = gaussian_2$prior$fn,
    n_dim = 2L
  )

  result <- create_live(lrps, 10)
  expect_equal(dim(result$unit), c(10, 2))
  expect_equal(
    apply(apply(result$unit, 1, gaussian_2$prior$fn), 2, gaussian_2$log_lik),
    result$log_lik
  )
})

test_that("Informative error when prior or log. lik. fails completely.", {
  bad_lik <- rwcube_lrps$new(
    log_lik_fn = \(x) stop("Bad Likelihood Job!"),
    prior_fn = gaussian_2$prior$fn,
    n_dim = 2L
  )
  bad_prior <- rwcube_lrps$new(
    log_lik_fn = gaussian_2$log_lik,
    prior_fn = \(x) stop("Bad prior job!"),
    n_dim = 2L
  )

  expect_snapshot_error(create_live(bad_lik, 10))
  expect_snapshot_error(create_live(bad_prior, 10))
})

test_that("check_live validates live points correctly", {
  # Test case: Valid live points
  unit <- matrix(runif(20, 0, 1), nrow = 5, ncol = 4)
  log_lik <- c(-10, -5, -3, -1, -0.5)
  expect_silent(check_live(unit, log_lik, n_points = 5, n_var = 4))

  # Test case: Empty live points
  unit <- matrix(numeric(0))
  log_lik <- numeric(0)
  expect_snapshot_error(check_live(unit, log_lik, n_points = 5, n_var = 4))

  # Test case: live$unit is not a matrix
  unit <- runif(20)
  log_lik <- c(-10, -5, -3, -1, -0.5)
  expect_snapshot_error(check_live(unit, log_lik, n_points = 5, n_var = 4))

  # Test case: live$unit dimensions do not match
  unit <- matrix(runif(15, 0, 1), nrow = 3, ncol = 5)
  log_lik <- c(-10, -5, -3, -1, -0.5)
  expect_snapshot_error(check_live(unit, log_lik, n_points = 5, n_var = 4))

  # Test case: live$unit contains non-finite values
  unit <- matrix(c(runif(19, 0, 1), Inf), nrow = 5, ncol = 4)
  log_lik <- c(-10, -5, -3, -1, -0.5)
  expect_snapshot_error(check_live(unit, log_lik, n_points = 5, n_var = 4))

  # Test case: live$unit contains values outside [0, 1]
  unit <- matrix(c(runif(19, 0, 1), 1.5), nrow = 5, ncol = 4)
  log_lik <- c(-10, -5, -3, -1, -0.5)
  expect_snapshot_error(check_live(unit, log_lik, n_points = 5, n_var = 4))

  # Test case: live$log_lik contains non-finite values
  unit <- matrix(runif(20, 0, 1), nrow = 5, ncol = 4)
  log_lik <- c(-10, -5, -3, -1, Inf)
  expect_snapshot_error(
    check_live(unit, log_lik, n_points = 5, n_var = 4)
  )

  # Test case: live$log_lik contains only one unique value
  unit <- matrix(runif(20, 0, 1), nrow = 5, ncol = 4)
  log_lik <- rep(-10, 5)
  expect_snapshot_error(check_live(unit, log_lik, n_points = 5, n_var = 4))
})
