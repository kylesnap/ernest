fn <- purrr::compose(gaussian_blobs$log_lik, gaussian_blobs$prior$fn)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
test_that("new_rwmh_cube errors on invalid arguments", {
  expect_snapshot_error(new_rwmh_cube(fn, 2L, target_acceptance = 0))
  expect_snapshot_error(new_rwmh_cube(fn, 2L, target_acceptance = 1))
  expect_snapshot_error(new_rwmh_cube(fn, 2L, steps = 1))
  expect_snapshot_error(new_rwmh_cube(fn, 2L, cov_fn = "fizz"))
})

test_that("rwmh_cube returns correct class and structure", {
  obj <- rwmh_cube()
  expect_s3_class(obj, c("rwmh_cube", "ernest_lrps"), exact = TRUE)
  expect_null(obj$unit_log_fn)
  expect_null(obj$n_dim)
  expect_equal(obj$max_loop, 1e6L)
  expect_equal(obj$target_acceptance, 0.5)
  expect_equal(obj$steps, 25)
  expect_identical(obj$cov_fn, stats::cov)
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
  expect_identical(diag(2), rwcube$cache$chol_cov)
})

rwcube <- new_rwmh_cube(fn, 2L, cov_fn = as_closure(stats::cov))
test_that("propose.rwmh_cube proposes a single new point", {
  result <- propose(rwcube, original = NULL, criteria = -Inf)
  expect_length(result$unit, 2L)
  expect_equal(
    gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
    result$log_lik
  )
  expect_snapshot(rwcube)
})

test_that("propose.rwmh_cube proposes a single point under a likelihood constraint", {
  result <- propose(rwcube, original = c(0.5, 0.5), criteria = -300)
  expect_length(result$unit, 2)
  expect_equal(rwcube$cache$n_call, 25L)
  expect_equal(
    gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
    result$log_lik
  )
  expect_snapshot(rwcube)

  result <- propose(rwcube, original = c(0.5, 0.5), criteria = Inf)
  expect_equal(result$log_lik, fn(c(0.5, 0.5)))
})

live <- replicate(500, propose(rwcube))
live <- do.call(rbind, live["unit", ])
chol_cov <- chol(stats::cov(live))
test_that("update_lrps resets and is idempotent for rwmh_cube", {
  acc_ratio <- rwcube$cache$n_accept / rwcube$cache$n_call
  new_rwcube <- update_lrps(rwcube, unit = live)
  expect_equal(new_rwcube$cache$n_call, 0L)
  expect_equal(new_rwcube$cache$n_accept, 0L)
  expect_equal(
    new_rwcube$cache$epsilon,
    exp((acc_ratio - 0.5) / 2L / 0.5)
  )
  expect_identical(rwcube$cache$chol_cov, chol_cov)
  newer_rwcube <- update_lrps(new_rwcube, unit = live)
  expect_identical(new_rwcube, newer_rwcube)
})

test_that("update_lrps warns when chol_cov can't be calculated", {
  bad_live <- matrix(0, nrow = 500, ncol = 2)
  expect_snapshot_error(update_lrps(rwcube, bad_live))
  expect_identical(diag(2), rwcube$cache$chol_cov)
})

test_that("Reproducible under seed", {
  set.seed(42L)
  result1 <- propose(rwcube, original = c(0.5, 0.5), criteria = -99.30685)

  set.seed(42L)
  result2 <- propose(rwcube, original = c(0.5, 0.5), criteria = -99.30685)
  expect_identical(result1, result2)
})

test_that("rwcube can accept alternate cov. methods", {
  set.seed(42L)
  alt_cov <- \(x) stats::cov(x, method = "spearman")
  rwcube_spear <- new_rwmh_cube(
    fn,
    2L,
    cov_fn = alt_cov
  )
  expect_identical(rwcube_spear$cov_fn, alt_cov)
  result <- propose(rwcube_spear, original = c(0.5, 0.5), criteria = -300)
  spear_chol_cov <- chol(stats::cov(live, method = "spearman"))
  update_lrps(rwcube_spear, unit = live)
  expect_identical(rwcube_spear$cache$chol_cov, spear_chol_cov)
})
