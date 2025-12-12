#' @srrstats {G5.7} These tests all demonstrate that change ernest's
#' computational parameters changes the behaviour of the NS algorithm
#' as expected.
reference_run <- expect_gaussian_run(rwmh_cube())

#' @srrstats {G5.6b, G5.9, G5.9a, G5.9b} Tests that parameters are
#' recovered under different seeds and with random noise added to the log-lik.
test_that("different seeds and noise levels don't impact evidence estimates", {
  expect_gaussian_run(rwmh_cube(), .seed = 24L)
  sqrt_eps <- sqrt(.Machine$double.eps)
  noisy_gaussian_blob_ll <- function(x) {
    ll <- gaussian_blobs$log_lik(x)
    ll + rnorm(1, mean = 0, sd = sqrt_eps)
  }
  expect_run(
    log_lik = noisy_gaussian_blob_ll,
    prior = gaussian_blobs$prior,
    sampler = rwmh_cube(),
    n_points = 100,
    .expected_log_z = gaussian_blobs$log_z_analytic,
    .seed = 42L
  )
})

#' @srrstats {BS4.6} Test checks that the NS convergence criteria
#' (min_logz) produce identical results to when the number of iterations
#' is set to a fixed value.
test_that("Convergence criteria behave as expected", {
  skip_extended()
  expect_gaussian_run(
    sampler = rwmh_cube(),
    .generate = list(max_iterations = reference_run$n_iter, min_logz = 0)
  )

  # Increasing n_points will increase iterations needed to converge
  run_500 <- expect_gaussian_run(sampler = rwmh_cube(), n_points = 500)
  expect_gt(run_500$n_iter, reference_run$n_iter)
})

#' @srrstats {BS4.7} Test checks that the NS converegence criteria (min_logz)
#' changes the number of iterations needed for the sampler to converge.
test_that("increasing min_logz reduces the iterations needed to converge", {
  skip_extended()
  run_short <- expect_gaussian_run(
    sampler = rwmh_cube(),
    .generate = list(min_logz = 0.1)
  )
  expect_gt(reference_run$n_iter, run_short$n_iter)
})

#' @srrstats {BS7.3} The scale of the prior should impact the iterations
#' needed for NS to converge an evidence estimate.
test_that("Using a more informative prior lowers time till convergence", {
  skip_extended()
  sampler_naive <- ernest_sampler(
    gaussian_blobs$log_lik,
    gaussian_blobs$prior,
    seed = 42
  )
  run_naive <- generate(sampler_naive)

  sampler_informed <- ernest_sampler(
    gaussian_blobs$log_lik,
    create_normal_prior(lower = -5, upper = 5, .n_dim = 2),
    seed = 42
  )
  run_informed <- generate(sampler_informed)
  expect_lt(run_informed$n_iter, run_naive$n_iter)
})
