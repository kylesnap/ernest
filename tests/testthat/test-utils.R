test_that("check_class works as expected", {
  expect_invisible(check_class(structure(list(), class = "foo"), "foo"))
  expect_invisible(
    check_class(structure(list(), class = c("foo", "bar")), "foo")
  )
  expect_invisible(
    check_class(structure(list(), class = c("foo", "bar")), c("foo", "bar"))
  )
  expect_invisible(check_class(NULL, "foo", allow_null = TRUE))
  expect_snapshot(check_class(1, "foo"), error = TRUE)
  expect_error(check_class(NULL, "foo", allow_null = FALSE), "not `NULL`")
})

test_that("check_matrix works as expected", {
  mat <- matrix(as.double(1:6), nrow = 2, ncol = 3)
  expect_invisible(check_matrix(mat, nrow = 2, ncol = 3))
  expect_error(
    check_matrix(mat, nrow = 3, ncol = 2),
    "`mat` must have 3 rows, not 2."
  )
  expect_error(
    check_matrix(matrix("a", 2, 2), nrow = 2, ncol = 2),
    "not a character matrix."
  )
  mat_nan <- mat
  mat_nan[1, 1] <- NaN
  expect_error(check_matrix(mat_nan, nrow = 2, ncol = 3), "finite")
  mat_low <- mat
  mat_low[1, 1] <- -10
  expect_error(
    check_matrix(mat_low, nrow = 2, ncol = 3, lower = 0),
    "must respect the lower bounds"
  )
  mat_up <- mat
  mat_up[1, 1] <- 100
  expect_error(
    check_matrix(mat_up, nrow = 2, ncol = 3, upper = 10),
    "must respect the upper bounds"
  )
})

test_that("check_unique_names works as expected", {
  expect_invisible(check_unique_names(list(a = 1, b = 2)))
  expect_snapshot(check_unique_names(list(a = 1, a = 2)), error = TRUE)
  expect_error(check_unique_names(list(1, 2)), "unique names")
  expect_error(check_unique_names(list(a = 1, 2)), "unique names")
})
