#' @ssrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("new_ernest_likelihood fails informatively", {
  expect_snapshot_error(new_ernest_likelihood(
    gaussian_blobs$log_lik,
    .nonfinite_action = "loudly"
  ))
})

test_that("create_likelihood with simple function", {
  test <- function(x) -sum((x - 1)^2)
  ll <- create_likelihood(test)

  expect_s3_class(ll, c("ernest_likelihood", "function"))
  expect_equal(attr(ll, "nonfinite_action"), "warn")
  attr(test, "is_rowwise") <- FALSE
  expect_identical(attr(ll, "unsafe_fn"), test)

  expect_equal(ll(c(1, 2, 3)), -5)
  expect_equal(ll(matrix(c(1, 2, 3), nrow = 1)), -5)
  expect_equal(
    ll(matrix(seq(1:9), byrow = TRUE, nrow = 3)),
    c(-5, -50, -149)
  )
  expect_snapshot(ll)
})

test_that("create_likelihood with rowwise_fn", {
  rowwise_fn <- \(x) {
    LaplacesDemon::dmvn(x, mu = c(-1, 1), Sigma = diag(0.95, nrow = 2))
  }

  ll <- create_likelihood(rowwise_fn = rowwise_fn)
  expect_s3_class(
    ll,
    c("ernest_likelihood", "purrr_function_partial", "function")
  )
  expect_equal(attr(ll, "nonfinite_action"), "warn")

  expect_s3_class(ll, c("ernest_likelihood", "function"))
  expect_equal(attr(ll, "nonfinite_action"), "warn")
  expect_equal(ll(c(1, 2)), rowwise_fn(c(1, 2)))
  expect_equal(ll(matrix(c(1, 2), nrow = 1)), rowwise_fn(c(1, 2)))
  expect_equal(
    ll(matrix(seq(0:5), byrow = TRUE, nrow = 3)),
    rowwise_fn(matrix(seq(0:5), byrow = TRUE, nrow = 3))
  )
  expect_snapshot(ll)
})

test_that("create_likelihood throws errors", {
  expect_snapshot_error(create_likelihood("fn"))

  test <- function(x) -sum((x - 1)^2)
  expect_snapshot_error(create_likelihood(test, .nonfinite_action = "blob"))
  expect_snapshot_error(create_likelihood(test, rowwise_fn = test))
})

test_that("create_likelihood can forward arguments", {
  ll <- create_likelihood(
    rowwise_fn = LaplacesDemon::dmvn,
    mu = c(-1, 1),
    Sigma = !!diag(0.95, nrow = 2)
  )
  ll_laplace <- \(x) {
    LaplacesDemon::dmvn(x, mu = c(-1, 1), Sigma = diag(0.95, nrow = 2))
  }

  expect_s3_class(ll, c("ernest_likelihood", "function"))
  expect_equal(attr(ll, "nonfinite_action"), "warn")
  expect_equal(ll(c(1, 2)), ll_laplace(c(1, 2)))
  expect_equal(ll(matrix(c(1, 2), nrow = 1)), ll_laplace(c(1, 2)))
  expect_equal(
    ll(matrix(seq(0:5), byrow = TRUE, nrow = 3)),
    ll_laplace(matrix(seq(0:5), byrow = TRUE, nrow = 3))
  )
  expect_snapshot(
    ll,
    transform = \(x) sub("<environment: .+>", "<environment ADDRESS>", x)
  )
})

test_that("create_likelihood can rebuild itself", {
  test <- function(x) -sum((x - 1)^2)
  ll <- create_likelihood(test)
  ll2 <- create_likelihood(ll)
  expect_identical(ll2, ll)

  ll <- create_likelihood(
    rowwise_fn = LaplacesDemon::dmvn,
    mu = c(-1, 1),
    Sigma = !!diag(0.95, nrow = 2)
  )
  ll2 <- create_likelihood(ll)
  expect_identical(ll2, ll)
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

  warn_ll <- create_likelihood(fn, .nonfinite_action = "warn")
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

  warn_ll <- create_likelihood(fn, .nonfinite_action = "warn")
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
