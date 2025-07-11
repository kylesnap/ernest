test_that("ernest_run as_draws_matrix", {
  run <- readRDS(test_path("./example_run.rds"))
  mat <- as_draws_matrix(run)

  n_iter <- run$n_iter + run$n_points

  expect_equal(dim(mat), c(n_iter, 3L))
  expect_equal(
    posterior::variables(mat),
    c("Uniform(-5, 5)", "Uniform(-5, 5).1")
  )

  expect_equal(
    log(weights(mat)),
    run$log_weight - max(run$log_evidence)
  )
})

test_that("ernest_run units", {
  run <- readRDS(test_path("./example_run.rds"))
  mat <- as_draws_matrix(run, units = "unit_cube")

  n_iter <- run$n_iter + run$n_points

  expect_equal(dim(mat), c(n_iter, 3L))
  expect_true(all(mat[, 1:2] > 0 & mat[, 1:2] < 1))
})

test_that("ernest_run radial coordinates", {
  run <- readRDS(test_path("./example_run.rds"))
  mat <- as_draws_matrix(run, radial = TRUE)

  n_iter <- run$n_iter + run$n_points

  expect_equal(dim(mat), c(n_iter, 4L))
  observed <- drop(mat[, ".radial"])
  attributes(observed) <- NULL
  expected <- sqrt(rowSums(mat[, c(1:2)]^2))
  attributes(expected) <- NULL
  expect_equal(
    unclass(observed),
    expected
  )
})

test_that("ernest_run to exported draws formats", {
  run <- readRDS(test_path("./example_run.rds"))

  expect_s3_class(as_draws(run), c("draws_matrix", "draws", "matrix"))
  expect_s3_class(as_draws_rvars(run), c("draws_rvars", "draws", "list"))
})

test_that("ernest_run to non-exported draws formats", {
  run <- readRDS(test_path("./example_run.rds"))

  expect_s3_class(as_draws(run), c("draws_matrix", "draws", "matrix"))
  expect_s3_class(
    posterior::as_draws_array(run),
    c("draws_array", "draws", "array")
  )
  expect_s3_class(
    posterior::as_draws_df(run),
    c("draws_df", "draws", "data.frame")
  )
})
