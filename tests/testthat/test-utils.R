test_that("as_scalar_integer works for valid input", {
  expect_equal(as_scalar_integer(5), 5L)
  expect_equal(as_scalar_integer(5L), 5L)
  expect_equal(as_scalar_integer(NA, allow_na = TRUE), NA_integer_)
  expect_equal(as_scalar_integer(NULL, allow_null = TRUE), integer(0))
  expect_error(as_scalar_integer(1.5), "integer")
  expect_error(as_scalar_integer("a"), "integer")
  expect_error(as_scalar_integer(5, min = 10), "`5` is not >= 10")
  expect_error(as_scalar_integer(5, max = 2), "`5` is not <= 2")
})

test_that("as_scalar_count works for valid input", {
  expect_equal(as_scalar_count(3), 3L)
  expect_equal(as_scalar_count(0, positive = FALSE), 0L)
  expect_equal(as_scalar_count(NA, allow_na = TRUE), NA_integer_)
  expect_equal(as_scalar_count(NULL, allow_null = TRUE), integer())
  expect_error(as_scalar_count(-1), "`-1` must be >= 1")
  expect_error(as_scalar_count(1.5), "count")
  expect_error(as_scalar_count("a"), "count")
})

test_that("as_scalar_double works for valid input", {
  expect_equal(as_scalar_double(5), 5)
  expect_equal(as_scalar_double(5L), 5)
  expect_equal(as_scalar_double(NA, allow_na = TRUE), NA_real_)
  expect_equal(as_scalar_double(NULL, allow_null = TRUE), NULL)
  expect_error(as_scalar_double("a"), "number")
  expect_error(as_scalar_integer(5, min = 10), "`5` is not >= 10")
  expect_error(as_scalar_integer(5, max = 2), "`5` is not <= 2")
})

test_that("as_scalar_logical works for valid input", {
  expect_equal(as_scalar_logical(TRUE), TRUE)
  expect_equal(as_scalar_logical(FALSE), FALSE)
  expect_equal(as_scalar_logical(NA, allow_na = TRUE), NA)
  expect_equal(as_scalar_logical(NULL, allow_null = TRUE), logical(0))
  expect_error(as_scalar_logical(1), "logical")
  expect_error(as_scalar_logical("a"), "logical")
  expect_error(as_scalar_logical(c(TRUE, FALSE)), "length")
})

test_that("as_univariate_function works for valid input", {
  expect_true(is.function(as_univariate_function(function(x) x + 1)))
  expect_true(is.function(as_univariate_function(sum)))
  expect_error(
    as_univariate_function(function(x, y) x + y),
    "`<fn>` must have exactly 1 formal arguments, but has 2."
  )
  expect_error(as_univariate_function(1), "function")
})

test_that("is_function works as expected", {
  expect_true(is_function(function(x) x))
  expect_true(is_function(sum))
  expect_error(is_function(1), "function")
})

test_that("is_class works as expected", {
  expect_true(is_class(structure(list(), class = "foo"), "foo"))
  expect_error(is_class(1, "foo"), "class")
})

test_that("which_minn works correctly", {
  x <- c(10, 20, 5, 15)
  expect_equal(which_minn(x, 1), 3)
  expect_equal(which_minn(x, 2), c(3, 1))
})
