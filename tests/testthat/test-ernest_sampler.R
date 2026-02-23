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

test_that("seed is preserved across runs when .seed is NA", {
  old_seed <- .Random.seed
  run1 <- expect_gaussian_run(
    sampler = rwmh_cube(),
    .seed = NA,
    .generate = list(max_iterations = 200L)
  )
  new_seed <- .Random.seed
  expect_identical(old_seed, new_seed)
})
