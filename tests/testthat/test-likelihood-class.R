test_that("create_likelihood with auto_batch", {
  fn <- function(x) -sum((x - 1)^2)
  ll <- create_likelihood(fn)
  expect_s3_class(ll, c("ernest_likelihood", "function"))
  expect_equal(attr(ll, "body"), fn)
  expect_equal(attr(ll, "error_action"), "abort")
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

  expect_equal(ll2(c(1, 2, 3)), -5)
  expect_equal(ll2(matrix(c(1, 2, 3), nrow = 1)), c(-5))
  expect_equal(
    ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
})

test_that("error_action options", {
  fn <- function(x) if (all(x >= 1)) -sum((x - 1)^2) else stop("Bad `x`")

  fail_ll <- create_likelihood(fn, error_action = "abort")
  expect_equal(fail_ll(c(1, 2, 3)), -5)
  expect_error(
    fail_ll(c(0, 1, 2)),
    "Caused by error in `fn\\(\\)`"
  )
  expect_equal(
    fail_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_error(
    fail_ll(matrix(c(0, 1, 2, 3, 4, 5), byrow = TRUE, nrow = 2)),
    "Caused by error in `FUN\\(\\)`"
  )

  warn_ll <- create_likelihood(fn, error_action = "warn")
  expect_equal(warn_ll(c(1, 2, 3)), -5)
  expect_warning(warn_ll(c(0, 1, 2)), "Caused by error in `fn\\(\\)`")
  expect_equal(
    warn_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_warning(
    res <- warn_ll(matrix(
      c(0, 1, 2, 1, 2, 3, 4, 5, 6),
      byrow = TRUE,
      nrow = 3
    )),
    "Caused by error in `FUN\\(\\)`"
  )
  expect_equal(res, c(-Inf, -Inf, -Inf))
})

test_that("non_finite action options", {
  fn <- function(x) if (all(x >= 1)) -sum((x - 1)^2) else return(NaN)

  fail_ll <- create_likelihood(fn, nonfinite_action = "abort")
  expect_equal(fail_ll(c(1, 2, 3)), -5)
  expect_error(
    fail_ll(c(0, 1, 2)),
    "Encountered NaN when evaluating likelihood."
  )
  expect_equal(
    fail_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_error(
    fail_ll(matrix(c(0, 1, 2, 3, 4, 5), byrow = TRUE, nrow = 2)),
    "Encountered NaN when evaluating likelihood."
  )

  warn_ll <- create_likelihood(fn, nonfinite_action = "warn")
  expect_equal(warn_ll(c(1, 2, 3)), -5)
  expect_warning(
    warn_ll(c(0, 1, 2)),
    "Replacing NaN with `-Inf`."
  )
  expect_equal(
    warn_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_warning(
    res <- warn_ll(matrix(
      c(0, 1, 2, 1, 2, 3, 4, 5, 6),
      byrow = TRUE,
      nrow = 3
    )),
    "Replacing NaN with `-Inf`."
  )
  expect_equal(res, c(-Inf, -5, -50))

  pass_ll <- create_likelihood(fn, nonfinite_action = "pass")
  expect_equal(pass_ll(c(1, 2, 3)), -5)
  expect_silent(pass_ll(c(0, 1, 2)))
  expect_equal(
    pass_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_silent(
    res <- pass_ll(matrix(
      c(0, 1, 2, 1, 2, 3, 4, 5, 6),
      byrow = TRUE,
      nrow = 3
    ))
  )
  expect_equal(res, c(-Inf, -5, -50))
})

test_that("create_likelihood with auto_batch = FALSE", {
  fn <- function(x) {
    if (is.vector(x)) {
      -sum((x - 1)^2)
    } else {
      drop(-rowSums((x - 1)^2))
    }
  }

  ll <- create_likelihood(fn)
  expect_s3_class(ll, c("ernest_likelihood", "function"))
  expect_equal(attr(ll, "body"), fn)
  expect_equal(attr(ll, "error_action"), "abort")
  expect_equal(attr(ll, "nonfinite_action"), "warn")

  expect_equal(ll(c(1, 2, 3)), -5)
  expect_equal(ll(matrix(c(1, 2, 3), nrow = 1)), c(-5))
  expect_equal(
    ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
})

test_that("error_action options with auto_batch = FALSE", {
  fn <- function(x) {
    if (is.vector(x)) {
      if (all(x >= 1)) -sum((x - 1)^2) else stop("Bad `x`")
    } else {
      apply(x, 1, function(row) {
        if (all(row >= 1)) -sum((row - 1)^2) else stop("Bad `x`")
      })
    }
  }

  fail_ll <- create_likelihood(fn, error_action = "abort", auto_batch = FALSE)
  expect_equal(fail_ll(c(1, 2, 3)), -5)
  expect_error(
    fail_ll(c(0, 1, 2)),
    "Caused by error in `fn\\(\\)`"
  )
  expect_equal(
    fail_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_error(
    fail_ll(matrix(c(0, 1, 2, 3, 4, 5), byrow = TRUE, nrow = 2)),
    "Caused by error in `FUN\\(\\)`"
  )

  warn_ll <- create_likelihood(fn, error_action = "warn", auto_batch = FALSE)
  expect_equal(warn_ll(c(1, 2, 3)), -5)
  expect_warning(warn_ll(c(0, 1, 2)), "Caused by error in `fn\\(\\)`")
  expect_equal(
    warn_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_warning(
    res <- warn_ll(matrix(
      c(0, 1, 2, 1, 2, 3, 4, 5, 6),
      byrow = TRUE,
      nrow = 3
    )),
    "Caused by error in `FUN\\(\\)`"
  )
  expect_equal(res, c(-Inf, -Inf, -Inf))
})

test_that("nonfinite_action options with auto_batch = FALSE", {
  fn <- function(x) {
    if (is.vector(x)) {
      if (all(x >= 1)) -sum((x - 1)^2) else return(Inf)
    } else {
      apply(x, 1, function(row) {
        if (all(row >= 1)) -sum((row - 1)^2) else return(Inf)
      })
    }
  }

  fail_ll <- create_likelihood(
    fn,
    nonfinite_action = "abort",
    auto_batch = FALSE
  )
  expect_equal(fail_ll(c(1, 2, 3)), -5)
  expect_error(
    fail_ll(c(0, 1, 2)),
    "Encountered Inf when evaluating likelihood."
  )
  expect_equal(
    fail_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_error(
    fail_ll(matrix(c(0, 1, 2, 3, 4, 5), byrow = TRUE, nrow = 2)),
    "Encountered Inf when evaluating likelihood."
  )

  warn_ll <- create_likelihood(
    fn,
    nonfinite_action = "warn",
    auto_batch = FALSE
  )
  expect_equal(warn_ll(c(1, 2, 3)), -5)
  expect_warning(
    warn_ll(c(0, 1, 2)),
    "Replacing Inf with `-Inf`."
  )
  expect_equal(
    warn_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_warning(
    res <- warn_ll(matrix(
      c(0, 1, 2, 1, 2, 3, 4, 5, 6),
      byrow = TRUE,
      nrow = 3
    )),
    "Replacing Inf with `-Inf`."
  )
  expect_equal(res, c(-Inf, -5, -50))

  pass_ll <- create_likelihood(
    fn,
    nonfinite_action = "pass",
    auto_batch = FALSE
  )
  expect_equal(pass_ll(c(1, 2, 3)), -5)
  expect_silent(pass_ll(c(0, 1, 2)))
  expect_equal(
    pass_ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_silent(
    res <- pass_ll(matrix(
      c(0, 1, 2, 1, 2, 3, 4, 5, 6),
      byrow = TRUE,
      nrow = 3
    ))
  )
  expect_equal(res, c(-Inf, -5, -50))
})
