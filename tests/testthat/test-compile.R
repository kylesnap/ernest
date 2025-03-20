lrps <- new_rwmh_cube(
  log_lik = gaussian_2$log_lik,
  prior_transform = gaussian_2$prior_transform,
  n_dim = 2,
  update_interval = 100,
  num_steps = 20,
  target_acceptance = 0.25,
  init_epsilon <- 1
)

test_that("compile can start from scratch", {
  sampler <- ernest_sampler$new(lrps, c("A", "B"), 100, FALSE)

  sampler <- compile(sampler)
  expect_equal(dim(sampler$live_points$units), c(100, 2))
  expect_equal(dim(sampler$live_points$points), c(100, 2))
  expect_length(sampler$live_points$log_lik, 100)
})

test_that("ErnestSampler can start from an existing run", {
  sampler <- nested_sampling(
    gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    ptype = 2L,
    n_points = 500L,
    sampler = rwmh_cube(),
    verbose = TRUE
  )
  sampler$compile()
  run <- generate(sampler, max_it = 1000)

  sampler2 <- sampler$clone(deep = TRUE)
  expect_message(
    sampler2$compile(refresh = FALSE),
    "existing live points"
  )

  expect_equal(sampler2$live, run$live_units)
  expect_false(obj_address(sampler2) == obj_address(sampler))
})

test_that("compile can refresh a sampler", {
  sampler <- ernest_sampler$new(lrps, c("A", "B"), 100, FALSE)
  sampler$compile()
  live_cpy <- sampler$live_points

  expect_no_message(sampler$compile(TRUE))
  sampler$generate(max_iterations = 10)

  expect_message(
    sampler$compile(TRUE),
    "Resetting"
  )

  expect_equal(dim(sampler$live_points$units), dim(live_cpy$units))
  expect_equal(dim(sampler$live_points$points), dim(live_cpy$points))
  expect_equal(length(sampler$live_points$log_lik), length(live_cpy$log_lik))
  expect_false(setequal(sampler$live_points, live_cpy))
})
