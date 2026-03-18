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
