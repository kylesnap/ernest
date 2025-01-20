test_that("unit_cube creates a valid unit_cube object", {
  sampler <- unit_cube()

  expect_s3_class(sampler, c("unit_cube", "ernest_sampler"))
  expect_null(sampler$log_lik)
  expect_null(sampler$prior_transform)
  expect_equal(sampler$num_dim, 0)
  expect_equal(sampler$name, "Unit Cube Sampler")
  expect_equal(sampler$description, "Uses the unit hypercube to bound the set of live points.")
})

test_that("unit_cube can be updated", {
  sampler <- unit_cube()
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior <- set_prior_transform(list(
    set_prior_transform(\(u) qnorm(u), "V1"),
    set_prior_transform(\(u) qnorm(u), "V2")
  ))

  updated_sampler <- update_sampler(
    sampler,
    log_lik = log_lik,
    prior_transform = prior,
    num_dim = 2
  )

  expect_s3_class(updated_sampler, c("unit_cube", "ernest_sampler"))
  expect_equal(updated_sampler$log_lik, log_lik)
  expect_equal(updated_sampler$prior_transform, prior)
  expect_equal(updated_sampler$num_dim, 2)
  expect_equal(updated_sampler$name, "Unit Cube Sampler")
  expect_equal(updated_sampler$description, "Uses the unit hypercube to bound the set of live points.")
})


