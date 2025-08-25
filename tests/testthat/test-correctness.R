#' Correctness Tests
#'
#' @srrstats {G5.4, G5.4b, G5.5} Running correctness tests against
#' results found by nestle with eggbox (see also test-algorithm for
#' more correctness checks).
test_that("Eggbox", {
  skip_extended_test()
  sampler <- ernest_sampler(eggbox$log_lik, eggbox$prior)
  run <- generate(sampler, seed = 42L)
  smry <- summary(run)

  expect_lt(
    abs(smry$log_evidence - eggbox$raster_z),
    3.0 * smry$log_evidence_err
  )
  expect_lt(
    abs(smry$log_evidence - eggbox$estimated_z),
    3.0 * smry$log_evidence_err
  )
})
