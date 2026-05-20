test_that("parallel ernest_likelihood works", {
  test_matrix <- matrix(c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), nrow = 2)

  ll <- parallel_likelihood(
    function(x) {
      sum(x) + c
    },
    c = 0.5
  )
  expect_s3_class(ll, c("ernest_likelihood", "crate"))
  expect_equal(attr(ll, "interface"), "scalar_fn")
  expect_equal(ll(c(0.0, 0.2, 0.4)), 1.1)
  expect_equal(ll(test_matrix), c(1.1, 1.4))

  mat_ll <- parallel_likelihood(
    vectorized_fn = function(x) {
      rowSums(x) + c
    },
    c = 0.5
  )
  expect_s3_class(mat_ll, c("ernest_likelihood", "crate"))
  expect_equal(attr(mat_ll, "interface"), "vectorized_fn")
  expect_equal(mat_ll(c(0.0, 0.2, 0.4)), 1.1)
  expect_equal(mat_ll(test_matrix), c(1.1, 1.4))
})

test_that("parallel ernest_prior works", {
  test_matrix <- matrix(c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), nrow = 2)

  pr <- parallel_prior(
    function(x) {
      cumsum(x) + c
    },
    .names = c("a", "b", "c"),
    c = 0.5
  )
  expect_s3_class(attr(pr, "body"), "crate")
  expect_equal(attr(pr, "interface"), "point_fn")
  expect_equal(
    pr$fn(c(0.0, 0.2, 0.4)),
    matrix(c(0.5, 0.7, 1.1), nrow = 1)
  )
  expect_equal(
    pr$fn(test_matrix),
    matrix(c(0.5, 0.6, 0.7, 0.9, 1.1, 1.4), nrow = 2)
  )

  mat_pr <- parallel_prior(
    vectorized_fn = function(x) {
      matrixStats::rowCumsums(x) + 0.5
    },
    .names = c("A", "B", "C")
  )
  expect_s3_class(attr(mat_pr, "body"), "crate")
  expect_equal(attr(mat_pr, "interface"), "vectorized_fn")
  expect_equal(
    pr$fn(c(0.0, 0.2, 0.4)),
    matrix(c(0.5, 0.7, 1.1), nrow = 1)
  )
  expect_equal(
    pr$fn(test_matrix),
    matrix(c(0.5, 0.6, 0.7, 0.9, 1.1, 1.4), nrow = 2)
  )
})
