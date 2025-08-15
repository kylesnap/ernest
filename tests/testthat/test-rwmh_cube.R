gaussian_2 <- make_gaussian(2)
fn <- purrr::compose(gaussian_2$log_lik, gaussian_2$prior$fn)

test_that("rwmh_cube returns correct class and structure", {
  obj <- rwmh_cube()
  expect_s3_class(obj, c("rwmh_cube", "ernest_lrps"), exact = TRUE)
  expect_null(obj$unit_log_fn)
  expect_null(obj$n_dim)
  expect_equal(obj$max_loop, 1e6L)
  expect_equal(obj$target_acceptance, 0.5)
  expect_equal(obj$steps, 25)
  expect_snapshot(obj)
})

test_that("new_rwmh_cube initializes correctly", {
  rwcube <- new_rwmh_cube(fn, 2L)
  expect_s3_class(rwcube, c("rwmh_cube", "ernest_lrps"), exact = TRUE)
  expect_identical(rwcube$unit_log_fn, fn)
  expect_equal(rwcube$max_loop, 1e6L)
  expect_equal(rwcube$n_dim, 2L)
  expect_equal(rwcube$target_acceptance, 0.5)
  expect_equal(rwcube$steps, 25)
})

test_that("new_rwmh_cube errors on invalid arguments", {
  expect_snapshot_error(new_rwmh_cube(fn, 2L, target_acceptance = 0))
  expect_snapshot_error(new_rwmh_cube(fn, 2L, target_acceptance = 1))
  expect_snapshot_error(new_rwmh_cube(fn, 2L, steps = 1))
})

rwcube <- new_rwmh_cube(fn, 2L)

test_that("propose.rwmh_cube proposes a single new point", {
  result <- propose(rwcube, original = NULL, criteria = -Inf)
  expect_equal(dim(result$unit), c(1, 2))
  expect_equal(
    gaussian_2$log_lik(gaussian_2$prior$fn(result$unit)),
    result$log_lik
  )
  expect_snapshot(rwcube)
})

test_that("propose.rwmh_cube proposes multiple new points with criteria", {
  test_val <- c(-37, -16, -9, -5, -3)
  result <- propose(rwcube, criteria = test_val)
  expect_equal(dim(result$unit), c(5, 2))
  expect_true(all(result$log_lik >= test_val))
  expect_equal(
    apply(result$unit, 1, \(x) gaussian_2$log_lik(gaussian_2$prior$fn(x))),
    result$log_lik
  )
  expect_snapshot(rwcube)
})

test_that("propose.rwmh_cube works with provided original points", {
  test_val <- c(-10.311600, -6.412965, -5.113419, -6.412965, -10.311600)
  original <- matrix(seq(0, 1, length = 10), ncol = 2)
  result <- propose(rwcube, original, test_val)
  expect_equal(dim(result$unit), c(5, 2))
  expect_true(all(result$log_lik >= test_val))
  expect_equal(
    apply(result$unit, 1, \(x) gaussian_2$log_lik(gaussian_2$prior$fn(x))),
    result$log_lik
  )
  expect_equal(rwcube$cache$n_call, 125L)
  expect_snapshot(rwcube)
})

test_that("propose.rwmh_cube returns original log_lik when criteria is Inf", {
  test_val <- c(Inf, Inf, Inf, Inf, Inf)
  original <- matrix(runif(5 * 2), ncol = 2)
  res <- propose(rwcube, original, criteria = test_val)
  expect_equal(
    res$log_lik,
    apply(original, 1, \(x) gaussian_2$log_lik(gaussian_2$prior$fn(x)))
  )
  expect_equal(rwcube$cache$n_call, 250L)
  expect_snapshot(rwcube)
})

test_that("update_lrps resets and is idempotent for rwmh_cube", {
  acc_ratio <- rwcube$cache$n_accept / rwcube$cache$n_call
  new_rwcube <- update_lrps(rwcube)
  expect_equal(new_rwcube$cache$n_call, 0L)
  expect_equal(new_rwcube$cache$n_accept, 0L)
  expect_equal(
    new_rwcube$cache$epsilon,
    exp((acc_ratio - 0.5) / 2L / 0.5)
  )
  newer_rwcube <- update_lrps(new_rwcube)
  expect_identical(new_rwcube, newer_rwcube)
})
