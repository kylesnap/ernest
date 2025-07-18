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
  # Valid case: correct matrix and log_lik
  unit <- matrix(runif(20), nrow = 5, ncol = 4)
  log_lik <- seq(-10, -2, length.out = 5)
  expect_silent(check_live(unit, log_lik, n_points = 5, n_var = 4))

  # Error: unit is not a matrix
  expect_snapshot_error(
    check_live(runif(20), log_lik, n_points = 5, n_var = 4)
  )

  # Error: unit has wrong dimensions
  unit_bad_dim <- matrix(runif(15), nrow = 3, ncol = 5)
  expect_snapshot_error(
    check_live(unit_bad_dim, log_lik, n_points = 5, n_var = 4)
  )

  # Error: unit contains non-finite values
  unit_nonfinite <- matrix(runif(20), nrow = 5, ncol = 4)
  unit_nonfinite[1, 1] <- NA
  expect_snapshot_error(
    check_live(unit_nonfinite, log_lik, n_points = 5, n_var = 4)
  )

  # Error: unit contains values outside [0, 1]
  unit_outside <- matrix(runif(20), nrow = 5, ncol = 4)
  unit_outside[2, 2] <- 1.5
  expect_snapshot_error(
    check_live(unit_outside, log_lik, n_points = 5, n_var = 4),
  )

  # Error: log_lik is not double of length n_points
  log_lik_short <- c(-10, -5, -3)
  expect_snapshot_error(
    check_live(unit, log_lik_short, n_points = 5, n_var = 4)
  )

  # Error: log_lik contains non-finite values (not -Inf)
  log_lik_nonfinite <- c(-10, -5, -3, -1, NaN)
  expect_snapshot_error(
    check_live(unit, log_lik_nonfinite, n_points = 5, n_var = 4)
  )

  # Error: log_lik is a plateau (all values identical)
  log_lik_plateau <- rep(-10, 5)
  expect_snapshot_error(
    check_live(unit, log_lik_plateau, n_points = 5, n_var = 4)
  )

  # Warning: log_lik has repeated values but not all identical
  log_lik_repeats <- c(-10, -5, -5, -5, -0.5)
  expect_snapshot_warning(
    check_live(unit, log_lik_repeats, n_points = 5, n_var = 4)
  )
})
