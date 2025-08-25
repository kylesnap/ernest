sampler <- ernest_sampler(
  log_lik = gaussian_blobs$log_lik,
  prior = gaussian_blobs$prior
)

test_that("create_live generates live points correctly", {
  result <- create_live(sampler$lrps, 10)
  expect_equal(dim(result$unit), c(10, 2))
  expect_equal(
    apply(
      apply(result$unit, 1, gaussian_blobs$prior$fn),
      2,
      gaussian_blobs$log_lik
    ),
    result$log_lik
  )
})

test_that("Informative error when prior or log. lik. fails completely.", {
  bad_lik <- new_rwmh_cube(
    unit_log_fn = purrr::compose(
      \(x) stop("Bad Likelihood Job!"),
      gaussian_blobs$prior$fn,
    ),
    n_dim = 2L
  )
  expect_snapshot_error(create_live(bad_lik, 10))

  bad_prior <- new_rwmh_cube(
    unit_log_fn = purrr::compose(
      gaussian_blobs$log_lik,
      \(x) stop("Bad Prior Job!")
    ),
    n_dim = 2L
  )
  expect_snapshot_error(create_live(bad_prior, 10))
})

#' @srrstats {BS2.1a} check_set_live() ensures that log_lik produces an expected
#' output given some output from the prior transformation, ensuring the two
#' are commensurate. It is run by compile and by ernest_sampler.
env_bind(
  sampler$run_env,
  unit = matrix(runif(1000), nrow = 500, ncol = 2),
  log_lik = seq(-10, -1, length.out = 500),
  birth = rep(0L, 500)
)

test_that("check_live_set catches problems in the live_env", {
  # Valid case: correct matrix and log_lik
  expect_silent(check_live_set(sampler))

  # Error: unit is not a matrix
  env_bind(sampler$run_env, unit = runif(20))
  expect_snapshot_error(check_live_set(sampler))
  env_bind(
    sampler$run_env,
    unit = matrix(runif(1000), nrow = 250, ncol = 4)
  )
  expect_snapshot_error(check_live_set(sampler))
  trick_mat <- matrix(runif(1000), nrow = 500, ncol = 2)
  trick_mat[5, 2] <- NaN
  env_bind(
    sampler$run_env,
    unit = trick_mat
  )
  expect_snapshot_error(check_live_set(sampler))
  trick_mat[5, 2] <- 0.5
  trick_mat[500, 1] <- 0
  env_bind(sampler$run_env, unit = trick_mat)
  expect_snapshot_error(check_live_set(sampler))

  # Log_lik problems
  too_short <- seq(-10, -1, length.out = 499)
  env_bind(
    sampler$run_env,
    unit = matrix(runif(1000), nrow = 500, ncol = 2),
    log_lik = too_short
  )
  expect_snapshot_error(check_live_set(sampler))
  nonfinite <- seq(-10, -1, length.out = 500)
  nonfinite[5] <- NaN
  env_bind(
    sampler$run_env,
    log_lik = nonfinite
  )
  expect_snapshot_error(check_live_set(sampler))
  nonfinite[5] <- Inf
  env_bind(
    sampler$run_env,
    log_lik = nonfinite
  )
  expect_snapshot_error(check_live_set(sampler))
  nonfinite[5] <- -Inf
  env_bind(sampler$run_env, log_lik = nonfinite)
  expect_no_error(check_live_set(sampler))

  # Error: log_lik is a plateau (all values identical)
  log_lik_plateau <- rep(-10, 500L)
  env_bind(sampler$run_env, log_lik = log_lik_plateau)
  expect_snapshot_error(check_live_set(sampler))

  # Warning: log_lik has repeated values but not all identical
  log_lik_repeats <- seq(-10, -1, length.out = 500)
  log_lik_repeats[250:500] <- log_lik_repeats[250]
  env_bind(sampler$run_env, log_lik = log_lik_repeats)
  expect_snapshot_warning(check_live_set(sampler))
})

test_that("compile method initializes live points", {
  sampler <- compile(sampler)
  orig_units <- sampler$run_env$unit
  orig_log_lik <- sampler$run_env$log_lik

  expect_equal(dim(orig_units), c(500, 2))
  expected_log_lik <- apply(
    t(apply(orig_units, 1, gaussian_blobs$prior$fn)),
    1,
    gaussian_blobs$log_lik
  )

  expect_equal(orig_log_lik, expected_log_lik)
  expect_equal(sampler$run_env$birth, rep(0L, 500))
  expect_snapshot(sampler)
})

test_that("Seed setting with ints produces expected matrices", {
  sampler <- compile(sampler, seed = 42L)
  matrix1 <- env_get(sampler$run_env, "unit")
  env_unbind(sampler$run_env, "unit")

  sampler <- compile(sampler, seed = 42L)
  matrix2 <- env_get(sampler$run_env, "unit")
  env_unbind(sampler$run_env, "unit")
  expect_identical(matrix1, matrix2)

  sampler <- compile(sampler, seed = NULL)
  matrix3 <- env_get(sampler$run_env, "unit")
  env_unbind(sampler$run_env, "unit")
  expect_false(identical(matrix1, matrix3))
})
