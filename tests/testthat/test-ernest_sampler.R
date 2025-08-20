test_that("ernest_sampler returns an ernest_sampler object", {
  prior <- create_uniform_prior(n_dim = 2, lower = -1, upper = 1)
  ll_fn <- function(x) -sum(x^2)
  sampler <- ernest_sampler(ll_fn, prior, n_points = 50)
  expect_s3_class(sampler, "ernest_sampler")
})

test_that("ernest_sampler errors with invalid prior", {
  ll_fn <- function(x) -sum(x^2)
  expect_snapshot_error(
    ernest_sampler(ll_fn, prior = \(x) runif(x), n_points = 10)
  )
})

#' @srrstats {BS2.13} Test that R can produce fully-verbose output when
#' requested (messages are turned off for all other tests).
test_that("Fully verbose output works", {
  log_lik <- gaussian_shell(2)
  prior <- create_uniform_prior(n_dim = 2, lower = -6, upper = 6)
  sampler <- ernest_sampler(log_lik, prior, n_points = 500)

  withr::local_options(rlib_message_verbosity = "verbose")

  expect_message(
    run <- generate(sampler, max_iterations = 1000, seed = 42L),
    regexp = "Creating new live points"
  )

  expect_snapshot(
    run2 <- generate(sampler, max_iterations = 2000, show_progress = FALSE)
  )
})
