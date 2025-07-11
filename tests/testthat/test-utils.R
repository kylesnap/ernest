test_that("validate_integer_parameter works correctly", {
  expect_equal(round_to_integer(5L, 10), 5L)
  expect_equal(round_to_integer(5.5, 10), 55L)

  expect_equal(round_to_integer(0.5, 10), 5L)
  expect_error(round_to_integer("a", 10), "must be numeric")
})

test_that("which_minn works correctly", {
  x <- c(10, 20, 5, 15)
  expect_equal(which_minn(x, 1), 3)
  expect_equal(which_minn(x, 2), c(3, 1))
})
