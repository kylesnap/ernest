test_that("check_class works as expected", {
  expect_invisible(check_class(structure(list(), class = "foo"), "foo"))
  expect_invisible(
    check_class(structure(list(), class = c("foo", "bar")), "foo")
  )
  expect_invisible(
    check_class(structure(list(), class = c("foo", "bar")), c("foo", "bar"))
  )
  expect_invisible(check_class(NULL, "foo", allow_null = TRUE))
  expect_snapshot_error(check_class(1, "foo"))
  expect_error(check_class(NULL, "foo", allow_null = FALSE), "not `NULL`")
})

test_that("check_matrix works as expected", {
  mat <- matrix(as.double(1:6), nrow = 2, ncol = 3)
  expect_invisible(check_matrix(mat, nrow = 2, ncol = 3))
  expect_error(check_matrix(mat, nrow = 3, ncol = 2), "dimensions")
  expect_error(check_matrix(matrix("a", 2, 2), nrow = 2, ncol = 2), "matrix")
  mat_nan <- mat
  mat_nan[1, 1] <- NaN
  expect_snapshot_error(check_matrix(mat_nan, nrow = 2, ncol = 3))
  mat_low <- mat
  mat_low[1, 1] <- -10
  expect_error(
    check_matrix(mat_low, nrow = 2, ncol = 3, lower = 0),
    "lower boundary"
  )
  mat_up <- mat
  mat_up[1, 1] <- 100
  expect_error(
    check_matrix(mat_up, nrow = 2, ncol = 3, upper = 10),
    "upper boundary"
  )
})

test_that("check_double works as expected", {
  expect_invisible(check_double(c(1, 2, 3), size = 3))
  expect_snapshot_error(check_double(c(1, 2, 3), size = 2))
  expect_error(check_double(c(1, NaN), size = 2), "missing")
  expect_error(check_double(c(1, Inf), size = 2), "Inf")
  expect_invisible(check_double(numeric(0), size = 0))
  expect_invisible(check_double(c(1, -Inf), size = 2, allow_neg_inf = TRUE))
  expect_error(
    check_double(c(1, -Inf), size = 2, allow_neg_inf = FALSE),
    "-Inf"
  )
  expect_error(check_double(list(1, 2), size = 2), "double vector")
})

test_that("check_unique_names works as expected", {
  expect_invisible(check_unique_names(list(a = 1, b = 2)))
  expect_snapshot_error(check_unique_names(list(a = 1, a = 2)))
  expect_error(check_unique_names(list(1, 2)), "unique names")
  expect_error(check_unique_names(list(a = 1, 2)), "unique names")
})

test_that("which_minn works correctly", {
  x <- c(10, 20, 5, 15)
  expect_equal(which_minn(x, 1), 3)
  expect_equal(which_minn(x, 2), c(3, 1))
})
