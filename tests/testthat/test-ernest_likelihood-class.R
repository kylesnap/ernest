test_that("create_likelihood.function works with valid input", {
  log_likelihood_fn <- function(x) -sum((x - 1)^2)
  wrapped_fn <- create_likelihood(log_likelihood_fn)
  expect_s3_class(wrapped_fn, c("ernest_likelihood", "function"))
  expect_true(is.function(attr(wrapped_fn, "body")))

  expect_equal(wrapped_fn(c(1, 2, 3)), -5)

  expect_s3_class(create_likelihood(wrapped_fn), "ernest_likelihood")
  expect_equal(
    create_likelihood(wrapped_fn),
    wrapped_fn
  )
})

test_that("create_likelihood.function throws errors when problematic", {
  log_likelihood_fn <- function(x) sum((x - 1)^2)
  wrapped_fn <- create_likelihood(log_likelihood_fn)

  expect_warning(
    {
      na <- wrapped_fn(c(1, 2, NA))
    },
    "`NA` to `-Inf`"
  )
  expect_warning(
    {
      inf <- wrapped_fn(c(1, 2, Inf))
    },
    "`Inf` to `-Inf`"
  )
  expect_warning(
    {
      nan <- wrapped_fn(c(1, 2, NaN))
    },
    "`NaN` to `-Inf`"
  )
  expect_equal(c(na, inf, nan), rep(-Inf, 3))
})
