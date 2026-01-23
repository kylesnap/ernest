set.seed(42)

test_that("ernest_sampler returns an ernest_sampler object", {
  prior <- create_uniform_prior(names = LETTERS[1:2], lower = -1, upper = 1)
  ll_fn <- function(x) -sum(x^2)
  sampler <- ernest_sampler(ll_fn, prior, nlive = 50, seed = 42)
  expect_s3_class(sampler, "ernest_sampler")
})

test_that("ernest_sampler errors with invalid prior", {
  ll_fn <- function(x) -sum(x^2)
  expect_snapshot_error(
    ernest_sampler(ll_fn, prior = \(x) runif(x), nlive = 10, seed = 42)
  )
})

#' @srrstats {BS2.13} Test that R can produce fully-verbose output when
#' requested (messages are turned off for all other tests).
cli::test_that_cli("Fully-verbose output", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  sampler <- ernest_sampler(
    gaussian_blobs$log_lik,
    gaussian_blobs$prior,
    nlive = 500,
    seed = 42
  )
  withr::local_options(rlib_message_verbosity = "verbose")
  expect_no_error(generate(sampler, max_iterations = 1000))
})
