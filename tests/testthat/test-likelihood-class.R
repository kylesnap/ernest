test_that("create_likelihood with auto_batch", {
  fn <- function(x) -sum((x - 1)^2)
  ll <- create_likelihood(fn)
  partial <- purrr::partial(fn)

  expect_s3_class(
    ll,
    c("ernest_likelihood", "purrr_function_partial", "function")
  )
  expect_equal(attr(ll, "nonfinite_action"), "warn")

  expect_equal(ll(c(1, 2, 3)), -5)
  expect_equal(ll(matrix(c(1, 2, 3), nrow = 1)), c(-5))
  expect_equal(
    ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )

  expect_snapshot(ll)
})

test_that("create_likelihood from an ernest_likelihood", {
  fn <- function(x) -sum((x - 1)^2)
  ll <- create_likelihood(fn)
  ll2 <- create_likelihood(ll)
  expect_equal(ll, ll2)

  expect_equal(ll2(c(1, 2, 3)), -5)
  expect_equal(ll2(matrix(c(1, 2, 3), nrow = 1)), c(-5))
  expect_equal(
    ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )

  expect_snapshot(ll)
})

test_that("non_finite action options", {
  fn <- function(x) if (all(x >= 1)) -sum((x - 1)^2) else NaN

  fail_ll <- create_likelihood(fn, .nonfinite_action = "abort")
  expect_equal(fail_ll(c(1, 2, 3)), -5)
  expect_snapshot_error(fail_ll(c(0, 1, 2)))

  expect_equal(
    fail_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_snapshot_error(fail_ll(matrix(
    c(0, 1, 2, 3, 4, 5),
    byrow = TRUE,
    nrow = 2
  )))

  warn_ll <- create_likelihood(fail_ll, .nonfinite_action = "warn")
  expect_equal(warn_ll(c(1, 2, 3)), -5)
  expect_snapshot_warning(warn_ll(c(0, 1, 2)))
  expect_equal(
    warn_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_snapshot_warning(
    res <- warn_ll(matrix(
      c(0, 1, 2, 1, 2, 3, 4, 5, 6),
      byrow = TRUE,
      nrow = 3
    ))
  )
  expect_equal(res, c(-Inf, -5, -50))

  pass_ll <- create_likelihood(fn, .nonfinite_action = "quiet")
  expect_equal(pass_ll(c(1, 2, 3)), -5)
  expect_no_message(pass_ll(c(0, 1, 2)))
  expect_equal(
    pass_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_no_message(
    res <- pass_ll(matrix(
      c(0, 1, 2, 1, 2, 3, 4, 5, 6),
      byrow = TRUE,
      nrow = 3
    ))
  )
  expect_equal(res, c(-Inf, -5, -50))
})

test_that("create_likelihood with auto_batch = FALSE", {
  rowwise_fn <- function(x) {
    if (is.vector(x)) {
      -sum((x - 1)^2)
    } else {
      drop(-rowSums((x - 1)^2))
    }
  }

  ll <- create_likelihood(rowwise_fn = rowwise_fn)
  expect_s3_class(
    ll,
    c("ernest_likelihood", "purrr_function_partial", "function")
  )
  expect_equal(attr(ll, "nonfinite_action"), "warn")

  expect_equal(ll(c(1, 2, 3)), -5)
  expect_equal(ll(matrix(c(1, 2, 3), nrow = 1)), c(-5))
  expect_equal(
    ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
})

test_that("nonfinite_action options with auto_batch = FALSE", {
  fn <- function(x) {
    if (is.vector(x)) {
      if (all(x >= 1)) -sum((x - 1)^2) else Inf
    } else {
      apply(x, 1, function(row) {
        if (all(row >= 1)) -sum((row - 1)^2) else Inf
      })
    }
  }

  fail_ll <- create_likelihood(fn, .nonfinite_action = "abort")
  expect_equal(fail_ll(c(1, 2, 3)), -5)
  expect_snapshot_error(fail_ll(c(0, 1, 2)))

  expect_equal(
    fail_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_snapshot_error(fail_ll(matrix(
    c(0, 1, 2, 3, 4, 5),
    byrow = TRUE,
    nrow = 2
  )))

  warn_ll <- create_likelihood(fail_ll, .nonfinite_action = "warn")
  expect_equal(warn_ll(c(1, 2, 3)), -5)
  expect_snapshot_warning(warn_ll(c(0, 1, 2)))
  expect_equal(
    warn_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_snapshot_warning(
    res <- warn_ll(matrix(
      c(0, 1, 2, 1, 2, 3, 4, 5, 6),
      byrow = TRUE,
      nrow = 3
    ))
  )
  expect_equal(res, c(-Inf, -5, -50))

  pass_ll <- create_likelihood(fn, .nonfinite_action = "quiet")
  expect_equal(pass_ll(c(1, 2, 3)), -5)
  expect_no_message(pass_ll(c(0, 1, 2)))
  expect_equal(
    pass_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_no_message(
    res <- pass_ll(matrix(
      c(0, 1, 2, 1, 2, 3, 4, 5, 6),
      byrow = TRUE,
      nrow = 3
    ))
  )
  expect_equal(res, c(-Inf, -5, -50))
})
