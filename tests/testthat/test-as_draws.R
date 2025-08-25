test_that("Fails gracefully with poor input", {
  data(example_run)
  expect_error(as_draws(example_run, units = "unitcube"))
  expect_error(as_draws(example_run, radial = 3L))
  expect_error(as_draws(example_run, unit_cube = TRUE))
})

test_that("ernest_run as_draws_matrix", {
  data(example_run)
  mat <- as_draws_matrix(example_run)

  n_iter <- example_run$n_iter + example_run$n_points

  expect_equal(dim(mat), c(n_iter, 4L))
  expect_equal(
    posterior::variables(mat),
    c("x", "y", "z")
  )

  expect_equal(
    log(weights(mat)),
    example_run$log_weight - max(example_run$log_evidence)
  )
})

test_that("ernest_run units", {
  data(example_run)
  mat <- as_draws_matrix(example_run, units = "unit_cube")

  n_iter <- example_run$n_iter + example_run$n_points

  expect_equal(dim(mat), c(n_iter, 4L))
  expect_true(all(mat[, 1:2] > 0 & mat[, 1:2] < 1))
})

test_that("ernest_run radial coordinates", {
  data(example_run)
  mat <- as_draws_matrix(example_run, radial = TRUE)

  n_iter <- example_run$n_iter + example_run$n_points

  expect_equal(dim(mat), c(n_iter, 5L)) # 3 + Weight, + Radial
  observed <- drop(mat[, ".radial"])
  attributes(observed) <- NULL
  expected <- sqrt(rowSums(mat[, c(1:3)]^2))
  attributes(expected) <- NULL
  expect_equal(
    unclass(observed),
    expected
  )
})

test_that("ernest_run to exported draws formats", {
  data(example_run)

  expect_s3_class(as_draws(example_run), c("draws_matrix", "draws", "matrix"))
  expect_s3_class(
    as_draws_rvars(example_run),
    c("draws_rvars", "draws", "list")
  )
})

test_that("ernest_run to non-exported draws formats", {
  data(example_run)

  expect_s3_class(as_draws(example_run), c("draws_matrix", "draws", "matrix"))
  expect_s3_class(
    posterior::as_draws_array(example_run),
    c("draws_array", "draws", "array")
  )
  expect_s3_class(
    posterior::as_draws_df(example_run),
    c("draws_df", "draws", "data.frame")
  )
})
