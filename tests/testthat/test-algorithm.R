#' Algorithmic Tests
#'
#' @srrstats {G5.7, BS5.3, BS7.3, BS4.6, BS4.7} Ensures that ernest aligns with
#' the expected behaviour of nested sampling (i.e., increasing the number of
#' points or the distance between the prior and posterior distributions increases the
#' number of iterations required for convergence). Also checks that min_logz and
#' max_iterations reaches the same evidence estimate.

log_lik <- gaussian_shell(2)
prior <- create_uniform_prior(n_dim = 2, lower = -6, upper = 6)
analytic_log_evidence <- -1.75

set.seed(42L)
sampler <- ernest_sampler(log_lik, prior, n_points = 500)
run_500 <- generate(sampler, seed = 42L)
smry_500 <- summary(run_500)

test_that("Ernest produces near-identical results when max_iter is used", {
  sampler <- ernest_sampler(log_lik, prior, n_points = 500)
  run_iter <- generate(
    sampler,
    max_iterations = smry_500$n_iter,
    min_logz = 0,
  )
  smry_iter <- summary(run_iter)

  expect_lt(
    smry_500$log_evidence - analytic_log_evidence,
    4 * smry_500$log_evidence_err
  )
  expect_lt(
    smry_iter$log_evidence - analytic_log_evidence,
    4 * smry_iter$log_evidence_err
  )
})

test_that("Increasing N increases the iterations needed for convergence", {
  skip_if(getOption("ernest.extended_tests", FALSE), "Skipping extended tests")
  sampler <- ernest_sampler(log_lik, prior, n_points = 1000)
  run_1000 <- generate(sampler, seed = 42)
  smry_1k <- summary(run_1000)

  expect_lt(
    smry_1k$log_evidence - analytic_log_evidence,
    4 * smry_1k$log_evidence_err
  )
  expect_lt(smry_500$n_iter, smry_1k$n_iter)
})

test_that("Increasing min_logz reduces the iterations needed to converge", {
  skip_if(getOption("ernest.extended_tests", FALSE), "Skipping extended tests")
  sampler <- ernest_sampler(log_lik, prior, n_points = 500)
  run_01 <- generate(sampler, min_logz = 0.1, seed = 42)
  smry_01 <- summary(run_01)

  expect_lt(
    smry_01$log_evidence - analytic_log_evidence,
    4 * smry_01$log_evidence_err
  )
  expect_lt(smry_01$n_iter, smry_500$n_iter)
})

test_that("Increasing the volume of the prior increases iterations", {
  skip_if(getOption("ernest.extended_tests", FALSE), "Skipping extended tests")
  wide_prior <- create_uniform_prior(n_dim = 2, lower = -10, upper = 10)

  sampler <- ernest_sampler(log_lik, wide_prior, n_points = 500)
  run_wide <- generate(sampler, seed = 42L)
  smry_wide <- summary(run_wide)

  expect_lt(
    smry_wide$log_evidence - analytic_log_evidence,
    4 * smry_wide$log_evidence_err
  )
  expect_lt(smry_500$n_iter, smry_wide$n_iter)
  expect_lt(
    tail(smry_500$run$information, 1L),
    tail(smry_wide$run$information, 1L)
  )
})
