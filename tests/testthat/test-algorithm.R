#' Algorithmic Tests

log_lik_fn <- gaussian_blobs$log_lik
unif_prior <- gaussian_blobs$prior

result <- NULL
smry_base <- NULL

#' @srrstats {G5.7} These tests all demonstrate that change ernest's
#' computational parameters changes the behaviour of the NS algorithm
#' as expected.

#' @srrstats {BS4.6} Test checks that the NS converegence criteria
#' (min_logz) produce identical results to when the number of iterations
#' is set to a fixed value.
result <- NULL
test_that("iterations and min_logz produce near-identical results", {
  sampler <- ernest_sampler(log_lik_fn, unif_prior, n_points = 100)
  result <<- generate(sampler)
  result_iter <- generate(
    sampler,
    max_iterations = result$n_iter,
    min_logz = 0
  )
  smry_iter <- summary(result_iter)

  expect_lt(
    abs(smry_iter$log_evidence - gaussian_blobs$analytic_z),
    3.0 * smry_iter$log_evidence_err
  )
})

test_that("`n_points` changes the iterations needed for convergence", {
  skip_extended_test()
  sampler_500 <- ernest_sampler(
    log_lik_fn,
    unif_prior,
    n_points = 500
  )
  run_500 <- generate(sampler_500)
  smry_500 <- summary(run_500)

  expect_lt(
    smry_500$log_evidence - gaussian_blobs$analytic_z,
    3 * smry_500$log_evidence_err
  )
  expect_gt(smry_500$n_iter, length(result$log_lik))
})

#' @srrstats {BS4.7} Test checks that the NS converegence criteria (min_logz)
#' changes the number of iterations needed for the sampler to converge.
test_that("increasing min_logz reduces the iterations needed to converge", {
  skip_extended_test()
  sampler <- ernest_sampler(log_lik_fn, unif_prior, n_points = 100)
  run_01 <- generate(sampler, min_logz = 0.1)
  smry_01 <- summary(run_01)

  expect_lt(
    smry_01$log_evidence - gaussian_blobs$analytic_z,
    3 * smry_01$log_evidence_err
  )
  expect_lt(smry_01$n_iter, length(result$log_lik))
})

#' @srrstats {BS7.3} The scale of the prior should impact the iterations
#' needed for NS to converge an evidence estimate.
test_that("increasing prior vol. increases iterations needed to converge", {
  skip_extended_test()
  wide_prior <- create_uniform_prior(lower = -10, upper = 10, .n_dim = 2)
  sampler_wide <- ernest_sampler(log_lik_fn, wide_prior, n_points = 500)
  run_wide <- generate(sampler_wide)
  smry_wide <- summary(run_wide)

  expect_lt(
    smry_wide$log_evidence - gaussian_blobs$analytic_z,
    3.0 * smry_wide$log_evidence_err
  )
  expect_lt(length(result$log_lik), smry_wide$n_iter)
  expect_lt(
    tail(result$information, 1L),
    tail(smry_wide$run$information, 1L)
  )
})
