#' Algorithmic Tests
set.seed(42)

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

  expect_evidence(result, gaussian_blobs$analytic_z, tolerance = 2)
})

test_that("`n_points` changes the iterations needed for convergence", {
  skip_extended_test()
  sampler_500 <- ernest_sampler(
    log_lik_fn,
    unif_prior,
    n_points = 500
  )
  run_500 <- generate(sampler_500)

  expect_evidence(run_500, gaussian_blobs$analytic_z, tolerance = 2)
  expect_gt(run_500$n_iter, result$n_iter)
})

#' @srrstats {BS4.7} Test checks that the NS converegence criteria (min_logz)
#' changes the number of iterations needed for the sampler to converge.
test_that("increasing min_logz reduces the iterations needed to converge", {
  skip_extended_test()
  sampler <- ernest_sampler(log_lik_fn, unif_prior, n_points = 100)
  run_01 <- generate(sampler, min_logz = 0.1)

  expect_evidence(run_01, gaussian_blobs$analytic_z, tolerance = 2)
  expect_lt(run_01$n_iter, result$n_iter)
})

#' @srrstats {BS7.3} The scale of the prior should impact the iterations
#' needed for NS to converge an evidence estimate.
test_that("increasing prior vol. increases iterations needed to converge", {
  skip_extended_test()
  wide_prior <- create_uniform_prior(lower = -10, upper = 10, .n_dim = 2)
  sampler_wide <- ernest_sampler(log_lik_fn, wide_prior, n_points = 500)
  run_wide <- generate(sampler_wide)

  expect_evidence(run_wide, gaussian_blobs$analytic_z, tolerance = 2)
  expect_gt(run_wide$n_iter, result$n_iter)
})
