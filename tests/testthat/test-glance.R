gauss <- make_gaussian(3L)
sampler <- nested_sampling(
  gauss$log_lik,
  prior_transform = gauss$prior_transform,
  n_dim = 3L
)

test_that("Glance on an empty sampler object", {
  expect_equal(
    glance(sampler),
    tibble::tibble(
      n_dim = 3L,
      n_points = 500L,
      n_iter = 0L,
      n_call = 0L
    )
  )
})

comp_samp <- compile(sampler)

test_that("Glance on a compiled sampler object", {
  expect_equal(
    glance(sampler),
    tibble::tibble(
      n_dim = 3L,
      n_points = 500L,
      n_iter = 0L,
      n_call = 0L
    )
  )
})

run <- generate(sampler, max_it = 1000L, refresh = TRUE)

test_that("Glance on a run with results", {
  expect_equal(
    names(glance(run)),
    c("n_dim", "n_points", "n_iter", "n_call", "eff", "log_z", "log_z_err", "information")
  )
  expect_equal(
    glance(run)[1:3],
    tibble::tibble(
      n_dim = 3L,
      n_points = 500L,
      n_iter = 1000L
    )
  )
})
