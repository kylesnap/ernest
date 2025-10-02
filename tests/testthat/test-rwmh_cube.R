fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

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

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
describe("new_rwmh_cube", {
  it("errors on invalid arguments", {
    expect_snapshot(new_rwmh_cube(fn, 2L, target_acceptance = 0), error = TRUE)
    expect_snapshot(new_rwmh_cube(fn, 2L, target_acceptance = 1), error = TRUE)
    expect_snapshot(new_rwmh_cube(fn, 2L, steps = 1), error = TRUE)
  })

  it("initializes correctly", {
    rwcube <- new_rwmh_cube(fn, 2L)
    expect_s3_class(rwcube, c("rwmh_cube", "ernest_lrps"), exact = TRUE)
    expect_identical(rwcube$unit_log_fn, fn)
    expect_equal(rwcube$max_loop, 1e6L)
    expect_equal(rwcube$n_dim, 2L)
    expect_equal(rwcube$target_acceptance, 0.5)
    expect_equal(rwcube$steps, 25)
  })
})

rwcube <- new_rwmh_cube(fn, 2L)
describe("propose.rwmh_cube", {
  it("proposes a single new point", {
    result <- propose(rwcube, original = NULL, criterion = -Inf)
    expect_length(result$unit, 2L)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_snapshot(rwcube)
  })

  it("proposes a single point under a likelihood constraint", {
    orig <- c(0.5, 0.5)
    result <- propose(rwcube, original = orig, criterion = -300)
    expect_length(result$unit, 2)
    expect_equal(rwcube$cache$n_call, 25L)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_equal(orig, c(0.5, 0.5))
  })

  it("is reproducible under seed", {
    set.seed(42L)
    result1 <- propose(rwcube, original = c(0.5, 0.5), criterion = -99.30685)

    set.seed(42L)
    result2 <- propose(rwcube, original = c(0.5, 0.5), criterion = -99.30685)
    expect_identical(result1, result2)
  })
})

live <- replicate(500, propose(rwcube))
live <- do.call(rbind, live["unit", ])
describe("update_lrps.rwmh_cube", {
  it("resets and is idempotent", {
    acc_ratio <- rwcube$cache$n_accept / rwcube$cache$n_call
    new_rwcube <- update_lrps(rwcube, unit = live)
    expect_equal(new_rwcube$cache$n_call, 0L)
    expect_equal(new_rwcube$cache$n_accept, 0L)
    expect_equal(
      new_rwcube$cache$epsilon,
      exp((acc_ratio - 0.5) / 2L / 0.5)
    )
    newer_rwcube <- update_lrps(new_rwcube, unit = live)
    expect_identical(new_rwcube, newer_rwcube)
  })
})
