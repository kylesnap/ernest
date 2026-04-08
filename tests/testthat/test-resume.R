#' @srrstats {G5.6b, G5.9, G5.9a, G5.9b} Tests that parameters are
#' recovered under different seeds and with random noise added to the log-lik.
test_that("different seeds and noise levels don't impact evidence estimates", {
  expect_gaussian_run(rwmh_cube(), .seed = 24L)
  sqrt_eps <- sqrt(.Machine$double.eps)
  noisy_gaussian_blob_ll <- function(x) {
    ll <- gaussian_blobs$log_lik(x)
    ll + rnorm(length(ll), mean = 0, sd = sqrt_eps)
  }
  expect_run(
    log_lik = create_likelihood(vectorized_fn = noisy_gaussian_blob_ll),
    prior = gaussian_blobs$prior,
    sampler = rwmh_cube(),
    nlive = 100,
    .expected_log_z = gaussian_blobs$log_z_analytic,
    .seed = NA
  )
})

test_that("Reproducing a ernest_sampler saved to disk", {
  withr::local_file("sampler.rds")
  sampler <- ernest_sampler(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior,
    seed = 42
  )
  run1 <- generate(sampler, max_iterations = 100)

  saveRDS(sampler, "sampler.rds")
  f_sampler <- readRDS("sampler.rds")
  f_run1 <- generate(f_sampler, max_iterations = 100)
  expect_identical(run1$log_volume, f_run1$log_volume)
  expect_identical(
    vctrs::field(run1$rcrd, "log_lik"),
    vctrs::field(f_run1$rcrd, "log_lik")
  )
  expect_identical(run1$rcrd, f_run1$rcrd)
})

test_that("`seed` is preserved across runs when set to NA", {
  old_seed <- .Random.seed
  run1 <- expect_gaussian_run(
    sampler = rwmh_cube(),
    .seed = NA,
    .generate = list(max_iterations = 200L)
  )
  new_seed <- .Random.seed
  expect_identical(old_seed, new_seed)
})
