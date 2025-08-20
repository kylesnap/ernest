#' Edge Case Testing
#'
#' @srrstats {G5.8} These all test that nested_sampling/generate warns or fails
#' gracefully when presented with certain likelihood structures.
NULL

#' Zero-Length Data
#'
#' @srrstats {G5.8a} Tests for when likelihood and prior presented by user return
#' zero-lengths.
test_that("Zero-length likelihood fails", {
  ll <- \(theta) double(0)
  prior <- create_uniform_prior(2)

  expect_snapshot_error(ernest_sampler(ll, prior))
})

test_that("Zero-length prior fails", {
  prior_fn <- \(theta) double(0)
  expect_snapshot_error(create_prior(prior_fn, n_dim = 0))
  expect_snapshot_error(create_prior(prior_fn, n_dim = 1))
})

#' Wrong types
#'
#' @srrstats {G5.8b} Ernest fails on non-numeric likelihood/prior outputs.
NULL

test_that("Fails on character types", {
  prior_fn <- \(theta) c("A", "B")
  expect_snapshot_error(create_prior(prior_fn, n_dim = 2))

  ll <- \(theta) if (theta[1] < 0) "L" else "U"
  expect_snapshot_error(ernest_sampler(ll, create_uniform_prior(2)))
})

test_that("Fails on complex types", {
  prior_fn <- \(theta) 0.15i * theta
  expect_snapshot_error(create_prior(prior_fn, n_dim = 2))

  ll <- \(theta) sum(0.15i * length(theta))
  expect_snapshot_error(ernest_sampler(ll, create_uniform_prior(2)))
})

#' Missing type
#'
#' @srrstats {G5.8c} ernest fails when NA are produced by a prior function.
test_that("Missing values in the prior", {
  prior_fn <- function(theta) ifelse(theta < 0.5, NaN, qunif(theta))
  expect_snapshot_error(create_prior(prior_fn, n_dim = 2))
})

#' @srrstats {BS2.14} Tests whether warnings are surpressed upon request.
#'
NULL

test_that("Missing values in the log-likelihood", {
  ll_fn <- gaussian_shell(2L)
  ll_fn_missing <- \(theta) {
    if (any(15 <= theta)) {
      return(NA)
    }
    ll_fn(theta)
  }
  prior <- create_uniform_prior(n_dim = 2, upper = 10 * pi)

  expect_snapshot_error(
    ernest_sampler(
      create_likelihood(ll_fn_missing, .nonfinite_action = "abort"),
      prior
    )
  )

  expect_no_message(
    ernest_sampler(
      create_likelihood(ll_fn_missing, .nonfinite_action = "quiet"),
      prior
    )
  )

  expect_snapshot_warning(
    ernest_sampler(
      create_likelihood(ll_fn_missing, .nonfinite_action = "warn"),
      prior
    )
  )
})

#' Special tests: Perfectly flat and nearly flat likelihoods
#'
#' @srrstats {G5.8d} Covers situations in which the likelihood starts as flat
#' (caught by compile) or becomes flat after many iterations.
test_that("Ernest fails when ll is flat to begin with", {
  ll <- \(theta) 0
  expect_snapshot_error(ernest_sampler(ll, create_uniform_prior(2)))
})

test_that("Ernest halts and warns when ll becomes flat during a run", {
  ll <- make_gaussian(2L)
  ll_flat <- \(theta) {
    if (any(theta > 0)) {
      return(0)
    }
    ll$log_lik(theta)
  }

  expect_warning(
    sampler <- ernest_sampler(ll_flat, prior = ll$prior),
    "`log_lik` may contain a likelihood plateau"
  )
  expect_snapshot(
    generate(sampler, seed = 42L)
  )
})
