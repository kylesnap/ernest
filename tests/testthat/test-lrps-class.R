fn <- purrr::compose(gaussian_blobs$log_lik, gaussian_blobs$prior$fn)
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
describe("new_ernest_lrps", {
  it("fails informatively", {
    expect_snapshot(new_ernest_lrps(unit_log_fn = 1), error = TRUE)
    expect_snapshot(new_ernest_lrps(fn, n_dim = 0), error = TRUE)
    expect_snapshot(new_ernest_lrps(fn, n_dim = 2, cache = 1), error = TRUE)

    local_options("ernest.max_loop" = 0L)
    expect_snapshot(new_ernest_lrps(fn, n_dim = 2), error = TRUE)
    local_options("ernest.max_loop" = Inf)
    expect_snapshot(new_ernest_lrps(fn, n_dim = 2), error = TRUE)
  })

  it("initializes correctly", {
    lrps <- new_ernest_lrps(fn, 2L)
    expect_s3_class(lrps, "ernest_lrps", exact = TRUE)
    expect_identical(lrps$unit_log_fn, fn)
    expect_equal(lrps$max_loop, 1e6L)
    expect_equal(lrps$n_dim, 2L)
    expect_equal(lrps$cache$n_call, 0L)

    local_options("ernest.max_loop" = 100L)
    obj <- new_ernest_lrps(fn, 2L)
    expect_equal(obj$max_loop, 100L)
    expect_snapshot(obj)
  })
})

describe("propose.ernest_lrps", {
  it("proposes a single new point", {
    lrps <- new_ernest_lrps(fn, 2L)
    result <- propose(lrps, original = NULL, criteria = -Inf)
    expect_length(result$unit, 2)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
  })

  it("errors if original is provided", {
    lrps <- new_ernest_lrps(fn, 2L)
    original <- matrix(runif(5 * 2), ncol = 2)
    expect_snapshot(
      propose(lrps, original, -1),
      error = TRUE
    )
  })

  it("is reproducible under seed", {
    set.seed(42L)
    lrps <- new_ernest_lrps(fn, 2L)
    result1 <- propose(lrps, criteria = -37)

    set.seed(42L)
    result2 <- propose(lrps, criteria = -37)
    expect_identical(result1, result2)
  })
})

test_that("update_lrps.ernest_lrps is idempotent", {
  lrps <- new_ernest_lrps(fn, 2L)
  lrps$cache$n_call <- 5L
  new_lrps <- update_lrps(lrps)
  expect_equal(new_lrps$cache$n_call, 0L)
  newer_lrps <- update_lrps(new_lrps)
  expect_identical(new_lrps, newer_lrps)
})
