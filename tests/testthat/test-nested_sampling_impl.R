#' @srrstats {G5.7} These tests all demonstrate that change ernest's
#' computational parameters changes the behaviour of the NS algorithm
#' as expected.
reference_run <- expect_gaussian_run(rwmh_cube())

#' @srrstats {BS4.6, BS7.3} Test checks that the NS convergence criteria
#' (min_logz) produce identical results to when the number of iterations
#' is set to a fixed value.
test_that("Convergence criteria behave as expected", {
  skip_extended()
  expect_gaussian_run(
    sampler = rwmh_cube(),
    .generate = list(max_iterations = reference_run$niter, min_logz = 0)
  )

  # Increasing nlive will increase iterations needed to converge
  run_500 <- expect_gaussian_run(sampler = rwmh_cube(), nlive = 500)
  expect_gt(run_500$niter, reference_run$niter)
})

#' @srrstats {BS4.7} Test checks that the NS converegence criteria (min_logz)
#' changes the number of iterations needed for the sampler to converge.
test_that("increasing min_logz reduces the iterations needed to converge", {
  skip_extended()
  run_short <- expect_gaussian_run(
    sampler = rwmh_cube(),
    .generate = list(min_logz = 0.1)
  )
  expect_gt(reference_run$niter, run_short$niter)
})

#' @srrstats {BS7.0, BS7.1} Parameter recovery for a normal distribution,
#' without any additional information beyond the normal log-likelihood dist.
test_that("Parameter recovery for a normal distribution", {
  prior <- create_normal_prior(mean = c(0, 0))
  log_l <- create_likelihood(
    \(x) {
      mvtnorm::dmvnorm(x, mean = c(0, 0), sigma = diag(2), log = TRUE)
    }
  )

  sampler <- ernest_sampler(log_l, prior, nlive = 100, seed = 42)
  run <- generate(sampler, max_iterations = 1000)
  draws <- as_draws(run) |> posterior::resample_draws()
  smry <- posterior::summarise_draws(
    draws,
    \(x) quantile(x, probs = c(0.05, 0.95))
  )

  expect_lte(-1.644854, smry[1, 2])
  expect_gte(1.644854, smry[1, 3])
  expect_lte(-1.644854, smry[2, 2])
  expect_gte(1.644854, smry[2, 3])
})
