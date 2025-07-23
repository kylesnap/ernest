test_that("nested_sampling returns an ernest_sampler object", {
  prior <- create_uniform_prior(n_dim = 2, lower = -1, upper = 1)
  ll_fn <- function(x) -sum(x^2)
  sampler <- nested_sampling(ll_fn, prior, n_points = 50)
  expect_s3_class(sampler, "ernest_sampler")
})

test_that("nested_sampling errors with invalid prior", {
  ll_fn <- function(x) -sum(x^2)
  expect_snapshot_error(
    nested_sampling(ll_fn, prior = \(x) runif(x), n_points = 10)
  )
})

#' @srrstats {BS2.12} Test for enabling verbose output.
cli::test_that_cli("Fully verbose output", {
  log_lik <- gaussian_shell(2)
  prior <- create_uniform_prior(n_dim = 2, lower = -6, upper = 6)
  sampler <- nested_sampling(log_lik, prior, n_points = 500)
  rlang::local_options(rlib_message_verbosity = "default")

  expect_snapshot({
    run <- generate(sampler, max_iterations = 1000, seed = 42L)
  })
})
