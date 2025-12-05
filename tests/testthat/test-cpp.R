run_cpp_tests("ernest")

test_that("PASS", {
  expect_equal(2 * 2, 4)
})
