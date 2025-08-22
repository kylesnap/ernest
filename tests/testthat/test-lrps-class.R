fn <- purrr::compose(gaussian_blobs$log_lik, gaussian_blobs$prior$fn)

#' @ssrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("new_ernest_lrps fails informativelys", {
  expect_snapshot_error(new_ernest_lrps(unit_log_fn = 1))
  expect_snapshot_error(new_ernest_lrps(fn, n_dim = 0))
  expect_snapshot_error(new_ernest_lrps(fn, n_dim = 2, cache = 1))

  local_options("ernest.max_loop" = 0L)
  expect_snapshot_error(new_ernest_lrps(fn, n_dim = 2))
  local_options("ernest.max_loop" = Inf)
  expect_snapshot_error(new_ernest_lrps(fn, n_dim = 2))
})


test_that("ernest_lrps class initializes correctly", {
  lrps <- new_ernest_lrps(fn, 2L)
  expect_s3_class(lrps, "ernest_lrps", exact = TRUE)
  expect_identical(lrps$unit_log_fn, fn)
  expect_equal(lrps$max_loop, 1e6L)
  expect_equal(lrps$n_dim, 2L)

  local_options("ernest.max_loop" = 100L)
  obj <- new_ernest_lrps(fn, 2L)
  expect_equal(obj$max_loop, 100L)
  expect_snapshot(obj)
})

test_that("propose.ernest_lrps proposes a single new point", {
  lrps <- new_ernest_lrps(fn, 2L)
  result <- propose(lrps, original = NULL, criteria = -Inf)
  expect_equal(dim(result$unit), c(1, 2))
  expect_equal(
    gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
    result$log_lik
  )
})

test_that("propose.ernest_lrps proposes multiple new points with criteria", {
  lrps <- new_ernest_lrps(fn, 2L)
  test_val <- c(-37, -16, -9, -5, -3)
  result <- propose(lrps, criteria = test_val)
  expect_equal(dim(result$unit), c(5, 2))
  expect_true(all(result$log_lik >= test_val))
  expect_equal(
    apply(result$unit, 1, \(x) {
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(x))
    }),
    result$log_lik
  )
})

test_that("propose.ernest_lrps errors if original is provided", {
  lrps <- new_ernest_lrps(fn, 2L)
  original <- matrix(runif(5 * 2), ncol = 2)
  expect_snapshot_error(
    propose(lrps, original, c(-1, -1, -1, -1, -1))
  )
})

test_that("update_lrps resets and is idempotent for ernest_lrps", {
  lrps <- new_ernest_lrps(fn, 2L)
  lrps$cache$n_call <- 5L
  new_lrps <- update_lrps(lrps)
  expect_equal(new_lrps$cache$n_call, 0L)
  newer_lrps <- update_lrps(new_lrps)
  expect_identical(new_lrps, newer_lrps)
})
