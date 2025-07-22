#' Algorithmic Tests
#'
#' @srrstats {G5.7, BS7.3} Ensures that ernest aligns with the expectations of
#' nested sampling (i.e., increasing the number of points or the distance
#' between the prior and posterior distributions increases the number of
#' iterations required for convergence).

log_lik <- gaussian_shell(2)
prior <- create_uniform_prior(n_dim = 2, lower = -6, upper = 6)
analytic_log_evidence <- -1.75

set.seed(42L)
sampler <- nested_sampling(log_lik, prior, n_points = 500)
run_500 <- generate(sampler)
smry_500 <- summary(run_500)

test_that("Increasing N increases the iterations needed for convergence", {
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()
  sampler <- nested_sampling(log_lik, prior, n_points = 1000)
  run_1000 <- generate(sampler, seed = 42)
  smry_1k <- summary(run_1000)

  expect_lt(
    smry_500$log_evidence - analytic_log_evidence,
    4 * smry_500$log_evidence_err
  )
  expect_lt(
    smry_1k$log_evidence - analytic_log_evidence,
    4 * smry_1k$log_evidence_err
  )
  expect_lt(smry_500$n_iter, smry_1k$n_iter)
})

test_that("Increasing the volume of the prior increases iterations", {
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()
  wide_prior <- create_uniform_prior(n_dim = 2, lower = -10, upper = 10)

  sampler <- nested_sampling(log_lik, wide_prior, n_points = 500)
  run_wide <- generate(sampler)
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
