fn <- purrr::compose(gaussian_blobs$log_lik, gaussian_blobs$prior$fn)

test_that("unif_cube returns correct class and structure", {
  obj <- unif_cube()
  expect_s3_class(obj, c("unif_cube", "ernest_lrps"), exact = TRUE)
  expect_null(obj$unit_log_fn)
  expect_null(obj$n_dim)
  expect_equal(obj$max_loop, 1e6L)
  expect_snapshot(obj)
})

test_that("new_unif_cube initializes correctly", {
  uniform <- new_unif_cube(fn, 2L, max_loop = 100)
  expect_s3_class(uniform, c("unif_cube", "ernest_lrps"), exact = TRUE)
  expect_identical(uniform$unit_log_fn, fn)
  expect_equal(uniform$max_loop, 100)
  expect_equal(uniform$n_dim, 2L)
})

uniform <- new_unif_cube(fn, 2L, max_loop = 100)
test_that("propose.unif_cube proposes a single new point", {
  result <- propose(uniform, original = NULL, criteria = -Inf)
  expect_equal(dim(result$unit), c(1, 2))
  expect_equal(
    gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
    result$log_lik
  )
  expect_snapshot(uniform)
})

test_that("propose.unif_cube proposes multiple new points with criteria", {
  test_val <- c(-100, -100, -100, -100, -100)
  result <- propose(uniform, criteria = test_val)
  expect_equal(dim(result$unit), c(5, 2))
  expect_true(all(result$log_lik >= test_val))
  expect_equal(
    apply(result$unit, 1, \(x) {
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(x))
    }),
    result$log_lik
  )
  expect_snapshot(uniform)
})

test_that("propose.unif_cube works with provided original points", {
  set.seed(42L)
  test_val <- c(-100, -100, -100, -100, -100)
  original <- matrix(runif(5 * 2), ncol = 2)
  result <- propose(uniform, original, test_val)
  expect_equal(dim(result$unit), c(5, 2))
  expect_true(all(result$log_lik >= test_val))
  expect_equal(
    apply(
      result$unit,
      1,
      \(x) gaussian_blobs$log_lik(gaussian_blobs$prior$fn(x))
    ),
    result$log_lik
  )
  n_call <- uniform$cache$n_call
  expect_gt(n_call, 1)
  expect_snapshot(uniform)
})

test_that("propose.unif_cube errors when max_loop is reached", {
  test_val <- c(Inf, Inf, Inf, Inf, Inf)
  expect_snapshot_error(propose(uniform, criteria = test_val))
})

test_that("update_lrps resets and is idempotent for unif_cube", {
  new_uniform <- update_lrps(uniform)
  expect_equal(new_uniform$cache$n_call, 0L)
  newer_uniform <- update_lrps(new_uniform)
  expect_identical(new_uniform, newer_uniform)
})
