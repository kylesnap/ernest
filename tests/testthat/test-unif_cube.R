fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

test_that("unif_cube returns correct class and structure", {
  obj <- unif_cube()
  expect_s3_class(obj, c("unif_cube", "ernest_lrps"), exact = TRUE)
  expect_null(obj$unit_log_fn)
  expect_null(obj$n_dim)
  expect_equal(obj$max_loop, 1e6L)
  expect_snapshot(obj)
})

describe("new_unif_cube", {
  it("initializes correctly", {
    uniform <- new_unif_cube(fn, 2L, max_loop = 100)
    expect_s3_class(uniform, c("unif_cube", "ernest_lrps"), exact = TRUE)
    expect_identical(uniform$unit_log_fn, fn)
    expect_equal(uniform$max_loop, 100)
    expect_equal(uniform$n_dim, 2L)
  })
})

uniform <- new_unif_cube(fn, 2L, max_loop = 10000)
describe("propose.unif_cube", {
  it("proposes a single new point", {
    result <- propose(uniform, original = NULL, criterion = -Inf)
    expect_length(result$unit, 2)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_snapshot(uniform)
  })

  it("evolves a single point", {
    result <- propose(
      uniform,
      original = c(0.5, 0.5),
      criterion = -99.3068
    )
    expect_length(result$unit, 2)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_gt(uniform$cache$n_call, 0)
    expect_snapshot(uniform)
  })
})

live <- replicate(500, propose(uniform))
live <- do.call(rbind, live["unit", ])
describe("update_lrps.unif_cube", {
  it("resets and is idempotent", {
    new_uniform <- update_lrps(uniform, live)
    expect_equal(new_uniform$cache$n_call, 0L)
    newer_uniform <- update_lrps(new_uniform)
    expect_identical(new_uniform, newer_uniform)
  })
})
