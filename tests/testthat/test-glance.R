lrps <- new_rwmh_cube(
  log_lik = gaussian_2$log_lik,
  prior_transform = gaussian_2$prior_transform,
  n_dim = 2,
  update_interval = 200,
  num_steps = 20,
  target_acceptance = 0.25,
  epsilon = 0.1
)

sampler <- ernest_sampler$new(
  lrps = lrps,
  ptype = c("A", "B"),
  n_points = 500
)

test_that("Glance on an empty sampler object", {
  expect_equal(
    glance(sampler),
    tibble::tibble(
      n_dim = 2L,
      n_points = 500L,
      n_iterations = 0L,
      n_calls = 0L
    )
  )
})

comp_samp <- compile(sampler)

test_that("Glance on a compiled sampler object", {
  expect_equal(
    glance(sampler),
    tibble::tibble(
      n_dim = 2L,
      n_points = 500L,
      n_iterations = 0L,
      n_calls = 0L
    )
  )
})

run <- generate(sampler, max_it = 1000L)

test_that("Glance on a run with results", {
  expect_equal(
    names(glance(run)),
    c("n_dim", "n_points", "n_iterations", "n_calls", "eff", "log_z", "log_z_err", "information")
  )
  expect_equal(
    glance(run)[1:3],
    tibble::tibble(
      n_dim = 2L,
      n_points = 500L,
      n_iterations = 1000L
    )
  )
})
