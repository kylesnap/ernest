lrps <- new_rwmh_cube(
  log_lik = gaussian_2$log_lik,
  prior_transform = gaussian_2$prior_transform,
  n_dim = 2,
  update_interval = 200,
  num_steps = 20,
  target_acceptance = 0.25,
  epsilon = 1
)

test_that("ernest_sampler initializes correctly", {
  workspace <- ernest_sampler$new(lrps, c("A", "B"), 500, FALSE)

  expect_equal(workspace$n_iterations, 0)
  expect_equal(workspace$n_calls, 0)
  expect_equal(workspace$live_points, list())
})

test_that("initializer fails and warns", {
  expect_error(
    ernest_sampler$new(gaussian_2$log_lik, c("A", "B"), 500),
    "class `ernest_lrps`"
  )
  expect_warning(
    ernest_sampler$new(lrps, c("A", "B"), 1),
    "less than twice the number of dimensions"
  )
  expect_error(
    ernest_sampler$new(lrps, c("A", "B"), 0),
    "larger than or equal to 1"
  )
  expect_error(
    ernest_sampler$new(lrps, c("A", "B"), 500, "POOF"),
    "`TRUE` or `FALSE`"
  )
})

test_that("Can generate", {
  sampler <- ernest_sampler$new(lrps, c("A", "B"), 100, FALSE)
  sampler <- compile(sampler)

  result <- sampler$generate(max_iterations = 100L)
  expect_equal(sampler$n_iterations, 100)
})
