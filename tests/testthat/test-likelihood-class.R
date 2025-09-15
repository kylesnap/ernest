#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("new_ernest_likelihood fails informatively", {
  expect_snapshot_error(new_ernest_likelihood(
    gaussian_blobs$log_lik,
    on_nonfinite = "loudly"
  ))
})

test_that("create_likelihood with simple function", {
  test <- function(x) -sum((x - 1)^2)
  ll <- create_likelihood(test)

  expect_s3_class(ll, c("ernest_likelihood", "function"))
  expect_equal(ll(c(1, 2, 3)), -5)
  expect_snapshot(ll)
})

test_that("create_likelihood throws errors", {
  expect_snapshot_error(create_likelihood("fn"))

  test <- function(x) -sum((x - 1)^2)
  expect_snapshot_error(create_likelihood(test, on_nonfinite = "blob"))
})

test_that("create_likelihood can rebuild itself", {
  test <- function(x) -sum((x - 1)^2)
  ll <- create_likelihood(test)
  ll2 <- create_likelihood(ll)
  expect_equal(ll(c(0.5, 0.5)), ll2(c(0.5, 0.5)))

  ll <- create_likelihood(
    \(x) {
      LaplacesDemon::dmvn(x, mu = c(-1, 1), Sigma = diag(0.95, nrow = 2))
    }
  )
  ll2 <- create_likelihood(ll)
  expect_equal(ll(c(0.5, 0.5)), ll2(c(0.5, 0.5)))
})

test_that("non_finite action options", {
  fn <- function(x) if (all(x >= 1)) -sum((x - 1)^2) else NaN

  fail_ll <- create_likelihood(fn, on_nonfinite = "abort")
  expect_equal(fail_ll(c(1, 2, 3)), -5)
  expect_snapshot_error(fail_ll(c(0, 1, 2)))

  warn_ll <- create_likelihood(fn, on_nonfinite = "warn")
  expect_equal(warn_ll(c(1, 2, 3)), -5)
  expect_snapshot_warning(warn_ll(c(0, 1, 2)))

  pass_ll <- create_likelihood(fn, on_nonfinite = "quiet")
  expect_equal(pass_ll(c(1, 2, 3)), -5)
  expect_no_message(result <- pass_ll(c(0, 1, 2)))
  expect_equal(result, -Inf)
})

test_that("fn fails if a non-double is returned", {
  fn <- function(x) if (all(x >= 1)) -sum((x - 1)^2) else "NA"

  fail_ll <- create_likelihood(fn, on_nonfinite = "abort")
  expect_equal(fail_ll(c(1, 2, 3)), -5)
  expect_snapshot_error(fail_ll(c(0, 1, 2)))

  warn_ll <- create_likelihood(fn, on_nonfinite = "warn")
  expect_equal(warn_ll(c(1, 2, 3)), -5)
  expect_snapshot_error(warn_ll(c(0, 1, 2)))

  pass_ll <- create_likelihood(fn, on_nonfinite = "quiet")
  expect_equal(pass_ll(c(1, 2, 3)), -5)
  expect_snapshot_error(result <- pass_ll(c(0, 1, 2)))
})
