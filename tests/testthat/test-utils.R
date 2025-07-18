test_that("which_minn works correctly", {
  x <- c(10, 20, 5, 15)
  expect_equal(which_minn(x, 1), 3)
  expect_equal(which_minn(x, 2), c(3, 1))
})
