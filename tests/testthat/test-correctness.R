#' Correctness Tests
#'
#' @srrstats {G5.4, G5.4b, G5.4c} These tests compare ernest against results
#' published in the Dynesty package.
#' @srrstats {BS7.4, BS7.4a} Also explicitly tests that output draws matrix
#' conforms to the scale of the prior.
NULL

test_that("Gaussian shells: 2D", {
  set.seed(42L)
  log_lik <- gaussian_shell(2)
  prior <- create_uniform_prior(
    n_dim = 2,
    lower = -6,
    upper = 6,
    varnames = c("x", "y")
  )

  sampler <- nested_sampling(log_lik, prior)
  run <- generate(sampler)
  smry <- summary(run)

  analytic_log_evidence <- -1.75
  expect_snapshot(smry)
  expect_lt(
    smry$log_evidence - analytic_log_evidence,
    4 * smry$log_evidence_err
  )

  draws <- as_draws(run)
  expect_lte(-6, min(posterior::extract_variable(draws, "x")))
  expect_lte(max(posterior::extract_variable(draws, "x")), 6)
  expect_lte(-6, min(posterior::extract_variable(draws, "y")))
  expect_lte(max(posterior::extract_variable(draws, "y")), 6)

  calc <- calculate(run, ndraws = 1000)
  sim_log_evidence <- tail(calc$log_evidence, 1)
  expect_lt(
    mean(sim_log_evidence - analytic_log_evidence),
    4 * posterior::sd(sim_log_evidence)
  )
})

test_that("Gaussian shells: 5D", {
  set.seed(42L)
  log_lik <- gaussian_shell(5)
  prior <- create_uniform_prior(n_dim = 5, lower = -6, upper = 6)

  sampler <- nested_sampling(log_lik, prior)
  run <- generate(sampler)
  smry <- summary(run)

  analytic_log_evidence <- -5.67
  expect_snapshot(smry)
  expect_lt(
    smry$log_evidence - analytic_log_evidence,
    4 * smry$log_evidence_err
  )

  calc <- calculate(run, ndraws = 1000)
  sim_log_evidence <- tail(calc$log_evidence, 1)
  expect_lt(
    mean(sim_log_evidence - analytic_log_evidence),
    4 * posterior::sd(sim_log_evidence)
  )
})

test_that("Gaussian shells: 10D", {
  set.seed(42L)
  log_lik <- gaussian_shell(10)
  prior <- create_uniform_prior(n_dim = 10, lower = -6, upper = 6)

  sampler <- nested_sampling(log_lik, prior)
  run <- generate(sampler)
  smry <- summary(run)

  analytic_log_evidence <- -14.59
  expect_snapshot(smry)
  expect_lt(
    smry$log_evidence - analytic_log_evidence,
    4 * smry$log_evidence_err
  )

  calc <- calculate(run, ndraws = 1000)
  sim_log_evidence <- tail(calc$log_evidence, 1)
  expect_lt(
    mean(sim_log_evidence - analytic_log_evidence),
    4 * posterior::sd(sim_log_evidence)
  )
})

test_that("Eggbox", {
  set.seed(42L)
  log_lik <- function(theta) {
    (2 + cos(theta[1] / 2) * cos(theta[2] / 2))^5
  }
  prior <- create_uniform_prior(n_dim = 2, upper = 10 * pi)

  sampler <- nested_sampling(log_lik, prior)
  run <- generate(sampler)
  smry <- summary(run)

  analytic_log_evidence <- 235.856
  expect_snapshot(smry)
  expect_lt(
    smry$log_evidence - analytic_log_evidence,
    4 * smry$log_evidence_err
  )
})
