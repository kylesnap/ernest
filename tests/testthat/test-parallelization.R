test_that("parallel ernest_likelihood works", {
  test_matrix <- matrix(c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), nrow = 2)

  ll <- parallel_likelihood(function(x) {
    sum(x)
  })
  it("produces scalar likelihoods", {
    expect_s3_class(ll, c("ernest_likelihood", "crate"))
    expect_equal(attr(ll, "interface"), "scalar_fn")
    expect_equal(ll(c(0.0, 0.2, 0.4)), 0.6)
    expect_equal(ll(test_matrix), c(0.6, 0.9))
  })

  mat_ll <- parallel_likelihood(
    vectorized_fn = function(x) {
      rowSums(x) + normalization
    },
    normalization = 0.5
  )
  it("produces likelihood from `vectorized_fn`", {
    expect_s3_class(mat_ll, c("ernest_likelihood", "crate"))
    expect_equal(attr(mat_ll, "interface"), "vectorized_fn")
    expect_equal(mat_ll(c(0.0, 0.2, 0.4)), 1.1)
    expect_equal(mat_ll(test_matrix), c(1.1, 1.4))
  })
})
