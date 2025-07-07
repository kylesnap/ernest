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
