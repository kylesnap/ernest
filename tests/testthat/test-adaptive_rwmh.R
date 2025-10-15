fn <- \(x) gaussian_blobs$log_lik(gaussian_blobs$prior$fn(x))
set.seed(42)

test_that("adaptive_rwmh returns correct class and structure", {
  obj <- adaptive_rwmh()
  expect_s3_class(obj, c("adaptive_rwmh", "ernest_lrps"), exact = TRUE)
  expect_null(obj$unit_log_fn)
  expect_null(obj$n_dim)
  expect_equal(obj$max_loop, 1e6L)
  expect_equal(obj$target_acceptance, 0.4)
  expect_equal(obj$steps, 25L)
  expect_equal(obj$min_epsilon, 0.1)
  expect_null(obj$strength)
  expect_equal(obj$forgetfulness, 0.4)
  expect_snapshot(obj)
})

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
describe("new_adaptive_rwmh", {
  it("errors on invalid arguments", {
    expect_snapshot(
      new_adaptive_rwmh(fn, 2L, target_acceptance = 0),
      error = TRUE
    )
    expect_snapshot(
      new_adaptive_rwmh(fn, 2L, target_acceptance = 1.1),
      error = TRUE
    )
    expect_snapshot(new_adaptive_rwmh(fn, 2L, steps = 1), error = TRUE)
    expect_snapshot(
      new_adaptive_rwmh(fn, 2L, target_acceptance = 0.01, steps = 25),
      error = TRUE
    )
    expect_snapshot(
      new_adaptive_rwmh(fn, 2L, forgetfulness = -0.1),
      error = TRUE
    )
    expect_snapshot(
      new_adaptive_rwmh(fn, 2L, forgetfulness = 1.1),
      error = TRUE
    )
    expect_snapshot(new_adaptive_rwmh(fn, 2L, min_epsilon = -1), error = TRUE)
    expect_snapshot(new_adaptive_rwmh(fn, 2L, strength = -1), error = TRUE)
  })

  it("initializes correctly", {
    adaptive_rw <- new_adaptive_rwmh(fn, 2L)
    expect_s3_class(
      adaptive_rw,
      c("adaptive_rwmh", "ernest_lrps"),
      exact = TRUE
    )
    expect_identical(adaptive_rw$unit_log_fn, fn)
    expect_equal(adaptive_rw$max_loop, 1e6L)
    expect_equal(adaptive_rw$n_dim, 2L)
    expect_equal(adaptive_rw$target_acceptance, 0.4)
    expect_equal(adaptive_rw$steps, 25)
    expect_equal(adaptive_rw$min_epsilon, 0.1)
    expect_equal(adaptive_rw$forgetfulness, 0.4)
    expect_equal(adaptive_rw$cache$mean, c(0.5, 0.5))
    expect_equal(adaptive_rw$cache$covariance, diag(1 / 12, nrow = 2))
  })
})

adaptive_rw <- new_adaptive_rwmh(fn, 2L)
describe("propose.adaptive_rwmh", {
  it("proposes a single new point", {
    result <- propose(adaptive_rw, original = NULL, criterion = -Inf)
    expect_length(result$unit, 2L)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
    expect_snapshot(adaptive_rw)
  })

  it("proposes a single point under a likelihood constraint", {
    orig <- c(0.5, 0.5)
    result <- propose(adaptive_rw, original = orig, criterion = -300)
    expect_length(result$unit, 2)
    expect_equal(adaptive_rw$cache$n_call, 25L)
    expect_equal(
      gaussian_blobs$log_lik(gaussian_blobs$prior$fn(result$unit)),
      result$log_lik
    )
  })
})

describe("update_lrps.adaptive_rwmh", {
  live <- replicate(500, propose(adaptive_rw))
  log_lik <- list_c(live["log_lik", ])
  live <- do.call(rbind, live["unit", ])
  adaptive_rw <<- update_lrps(adaptive_rw, unit = live)

  it("sets the covariance matrix and is idempotent", {
    cov <- stats::cov(live)
    expect_equal(adaptive_rw$cache$n_call, 0L)
    expect_equal(adaptive_rw$cache$n_accept, 0L)
    expect_equal(adaptive_rw$strength, 500)
    expect_equal(adaptive_rw$cache$covariance, cov)
    newer_rw <- update_lrps(adaptive_rw, unit = live)
    expect_identical(newer_rw, adaptive_rw)
  })

  it("targets the acceptance ratio", {
    live <- live[order(log_lik), ]
    log_lik <- sort(log_lik)
    actual_acceptance <- double(499)
    for (i in seq(499)) {
      res <- propose(
        adaptive_rw,
        original = live[i + 1, ],
        criterion = log_lik[i]
      )
      actual_acceptance[i] <- (res$n_accept / res$n_call)
    }
    expect_lt(mean(actual_acceptance) - 0.4, 2 * sd(actual_acceptance))

    more_strict <- new_adaptive_rwmh(fn, 2L, target_acceptance = 0.1)
    more_strict <- update_lrps(more_strict, unit = live)
    for (i in seq(499)) {
      res <- propose(
        more_strict,
        original = live[i + 1, ],
        criterion = log_lik[i]
      )
      actual_acceptance[i] <- (res$n_accept / res$n_call)
    }
    expect_lt(mean(actual_acceptance) - 0.1, 2 * sd(actual_acceptance))
    expect_snapshot(more_strict)

    less_strict <- new_adaptive_rwmh(fn, 2L, target_acceptance = 0.9)
    less_strict <- update_lrps(less_strict, unit = live)
    for (i in seq(499)) {
      res <- propose(
        less_strict,
        original = live[i + 1, ],
        criterion = log_lik[i]
      )
      actual_acceptance[i] <- (res$n_accept / res$n_call)
    }
    expect_lt(mean(actual_acceptance) - 0.9, 2 * sd(actual_acceptance))
    expect_snapshot(less_strict)
  })
})
