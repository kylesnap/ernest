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

  expect_error(
    nested_sampling(ll, prior),
    "Failed sanity check."
  )
})

test_that("Zero-length prior fails", {
  prior_fn <- \(theta) double(0)

  expect_error(
    create_prior(prior_fn, n_dim = 0),
    "`n_dim` must be >= 1"
  )

  expect_error(
    create_prior(prior_fn, n_dim = 1),
    "must have length 1, but has length 0."
  )
})

#' Wrong types
#'
#' @srrstats {G5.8b} Ernest fails on non-numeric likelihood/prior outputs.
NULL

test_that("Fails on character types", {
  prior_fn <- \(theta) c("A", "B")
  expect_error(
    create_prior(prior_fn, n_dim = 2),
    "must be of type 'numeric', not 'character'"
  )

  ll <- \(theta) if (theta[1] < 0) "L" else "U"
  expect_error(
    nested_sampling(ll, create_uniform_prior(2)),
    "Failed sanity check."
  )
  expect_warning(
    nested_sampling(ll, create_uniform_prior(2), on_warning = "warn"),
    "Sanity check caused a warning."
  )
})

test_that("Fails on complex types", {
  prior_fn <- \(theta) c(1i * theta) / 10
  expect_error(
    create_prior(prior_fn, n_dim = 2),
    "must be of type 'numeric', not 'complex'."
  )

  ll <- \(theta) sum(c(1i * theta) / 10)
  expect_error(
    nested_sampling(ll, create_uniform_prior(2)),
    "Failed sanity check."
  )
  expect_warning(
    nested_sampling(ll, create_uniform_prior(2), on_warning = "warn"),
    "Sanity check caused a warning."
  )
})

#' Missing type
#'
#' @srrstats {G5.8c} ernest fails when NA are produced by a prior, but
#' can replace missing values produced by the log-likelihood.
NULL

test_that("Missing values in the prior", {
  prior_fn <- function(theta) ifelse(theta < 0.5, NaN, qunif(theta))
  expect_error(
    create_prior(prior_fn, 2L),
    "contains missing values"
  )

  prior_fn <- function(theta) ifelse(theta < 0.5, NA, qunif(theta))
  expect_error(
    create_prior(prior_fn, 2L),
    "contains missing values"
  )
})

test_that("Missing values in the log-likelihood", {
  ll_fn <- gaussian_shell(2L)
  ll_fn_missing <- \(theta) {
    if (any(15 <= theta & theta < 25)) {
      return(NA)
    }
    ll_fn(theta)
  }

  prior <- create_uniform_prior(n_dim = 2, upper = 10 * pi)
  expect_error(
    nested_sampling(ll_fn_missing, prior),
    "Failed sanity check."
  )
  expect_warning(
    nested_sampling(ll_fn_missing, prior, on_warning = "warn"),
    "Replacing `NA` with `-Inf`."
  )
  expect_no_message({
    sampler <- nested_sampling(
      ll_fn_missing,
      prior,
      on_warning = "warn",
      nonfinite_action = "pass"
    )
    generate(sampler, max_iterations = 3000)
  })
})

#' Special tests: Perfectly flat and nearly flat likelihoods
#'
#' @srrstats {G5.8d} Covers situations in which the likelihood starts as flat
#' (caught by compile) or becomes flat after many iterations.
NULL

test_that("Ernest fails when ll is flat to begin with", {
  ll <- \(theta) 0
  sampler <- nested_sampling(ll, create_uniform_prior(2))

  expect_error(
    generate(sampler, max_iterations = 100),
    "Log likelihood of all 500 points = 0."
  )
})

test_that("Ernest halts and warns when ll becomes flat during a run", {
  set.seed(42)
  ll <- make_gaussian(2L)
  ll_flat <- \(theta) {
    if (any(theta > 0)) {
      return(0)
    }
    ll$log_lik(theta)
  }

  sampler <- nested_sampling(ll_flat, prior = ll$prior)
  expect_snapshot(
    generate(sampler)
  )
})
