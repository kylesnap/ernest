fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

test_that("slice returns correct class and structure", {
  obj <- slice()
  expect_s3_class(obj, c("slice", "ernest_lrps"), exact = TRUE)
  expect_null(obj$unit_log_fn)
  expect_null(obj$n_dim)
  expect_equal(obj$enlarge, 1.0)
  expect_snapshot(obj)
})

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
describe("new_slice", {
  it("errors on invalid arguments", {
    expect_snapshot(new_slice(fn, 2L, enlarge = 0.5), error = TRUE)
    expect_snapshot(slice(enlarge = 0), error = TRUE)
    expect_snapshot(slice(enlarge = "invalid"), error = TRUE)
  })

  it("initializes correctly", {
    slc <- new_slice(fn, 2L)
    expect_s3_class(slc, c("slice", "ernest_lrps"), exact = TRUE)
    expect_identical(slc$unit_log_fn, fn)
    expect_equal(slc$n_dim, 2L)
    expect_equal(slc$enlarge, 1.0)
    expect_equal(slc$cache$lower, c(0.0, 0.0))
    expect_equal(slc$cache$upper, c(1.0, 1.0))
  })

  it("allows NA enlarge parameter", {
    slc <- slice(enlarge = NA)
    expect_true(is.na(slc$enlarge))
  })
})

slc <- new_slice(fn, 2L)
describe("propose.slice", {
  it("proposes a single new point", {
    result <- propose(slc, original = NULL, criterion = -Inf)
    expect_length(result$unit, 2L)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_snapshot(slc)
  })

  it("proposes a single point under a likelihood constraint", {
    orig <- c(0.5, 0.5)
    result <- propose(slc, original = orig, criterion = -99.30685)
    expect_length(result$unit, 2)
    expect_gt(slc$cache$n_call, 0L)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_gte(result$log_lik, -99.30685)
    expect_equal(orig, c(0.5, 0.5))
    expect_length(result$rect$lower, 2)
    expect_length(result$rect$upper, 2)
    expect_true(all(result$rect$lower <= result$unit))
    expect_true(all(result$rect$upper >= result$unit))
  })

  it("is reproducible under seed", {
    set.seed(42L)
    result1 <- propose(slc, original = c(0.5, 0.5), criterion = -99.30685)

    set.seed(42L)
    result2 <- propose(slc, original = c(0.5, 0.5), criterion = -99.30685)
    expect_identical(result1, result2)
  })
})

live <- replicate(500, propose(slc))
live <- do.call(rbind, live["unit", ])
describe("update_lrps.slice", {
  it("resets and updates bounding rectangle", {
    new_slc <- update_lrps(slc, unit = live)
    expect_equal(new_slc$cache$n_call, 0L)
    expect_equal(new_slc$cache$n_accept, 0L)
    expect_length(new_slc$cache$lower, 2)
    expect_length(new_slc$cache$upper, 2)
    expect_true(all(new_slc$cache$lower >= 0.0))
    expect_true(all(new_slc$cache$upper <= 1.0))
    expect_true(all(new_slc$cache$lower <= apply(live, 2, min)))
    expect_true(all(new_slc$cache$upper >= apply(live, 2, max)))
  })

  it("enlarges rectangle when enlarge > 1", {
    slc_enlarge <- new_slice(fn, 2L, enlarge = 1.5)
    new_slc <- update_lrps(slc_enlarge, unit = live)

    mins <- apply(live, 2, min)
    maxs <- apply(live, 2, max)
    center <- (mins + maxs) / 2
    radius <- (maxs - mins) / 2
    new_radius <- radius * 1.5^(1 / 2)

    expected_lower <- pmax(center - new_radius, 0.0)
    expected_upper <- pmin(center + new_radius, 1.0)

    expect_equal(new_slc$cache$lower, expected_lower)
    expect_equal(new_slc$cache$upper, expected_upper)
  })
})
