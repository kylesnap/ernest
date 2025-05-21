prior <- ernest_prior(gaussian_2$prior_transform)
prior_fn <- compile(
  ernest_prior(gaussian_2$prior_transform)
)

test_that("ernest_sampler initializes correctly", {
  sampler <- ernest_sampler$new(
    log_lik_fn = gaussian_2$log_lik,
    prior = prior,
    sampler = rwmh_cube(),
    n_points = 500,
    first_update = 200L,
    update_interval = 50L
  )
  expect_equal(sampler$niterations, 0)
  expect_equal(sampler$ncalls, 0)
  expect_message(
    {
      live <- sampler$get_live_points()
    },
    "No live points have been generated yet"
  )
  expect_mapequal(
    live,
    tibble::tibble(A = double(0), B = double(0))
  )
  expect_message(
    {
      dead <- sampler$get_dead_points()
    },
    "No dead points have been generated yet"
  )
  expect_mapequal(
    dead,
    tibble::tibble(A = double(0), B = double(0))
  )
  expect_warning(
    {
      calc <- expect_null(sampler$calculate())
    },
    "No iterations have been performed yet"
  )
  expect_null(calc)
  expect_snapshot(sampler)
})

test_that("compile method initializes live points", {
  prior_fn <- compile(ernest_prior(gaussian_2$prior_transform))
  sampler <- ernest_sampler$new(
    log_lik_fn = gaussian_2$log_lik,
    prior = prior,
    sampler = rwmh_cube(),
    n_points = 500,
    first_update = 200L,
    update_interval = 50L
  )
  sampler$compile()
  expect_equal(sampler$niterations, 0)
  expect_equal(sampler$ncalls, 0)
  orig_points <- sampler$get_live_points(units = "original")
  orig_units <- sampler$get_live_points(units = "unit")
  expect_equal(nrow(orig_points), 500)
  expect_equal(
    as.matrix(orig_points),
    t(apply(as.matrix(orig_units), 1, prior_fn))
  )
  expect_message(
    {
      dead <- sampler$get_dead_points()
    },
    "No dead points have been generated yet"
  )
  expect_warning(
    sampler$calculate(),
    "No iterations have been performed yet"
  )
  expect_snapshot(sampler)
})

sampler <- ernest_sampler$new(
  log_lik_fn = gaussian_2$log_lik,
  prior = prior,
  sampler = rwmh_cube(),
  n_points = 500,
  first_update = 200L,
  update_interval = 50L
)
test_that("generate method performs sampling", {
  set.seed(42)
  sampler$generate(max_iterations = 100)
  expect_equal(sampler$niterations, 100)
  expect_equal(sampler$ncalls, 2500)
  orig_points <- sampler$get_live_points(units = "original")
  orig_units <- sampler$get_live_points(units = "unit")
  expect_equal(nrow(orig_points), 500)
  expect_equal(
    as.matrix(orig_points),
    t(apply(as.matrix(orig_units), 1, prior_fn))
  )
  expect_equal(
    nrow(sampler$get_dead_points()),
    100
  )
  expect_equal(
    as.matrix(sampler$get_dead_points()),
    t(apply(as.matrix(sampler$get_dead_points("unit")), 1, prior_fn))
  )
  expect_snapshot(sampler)
})

test_that("calculate method returns correct evidence integral", {
  calc <- sampler$calculate()
  expect_equal(
    nrow(sampler$calculate()),
    600
  )
  expect_named(
    calc,
    c(
      "log_likelihood",
      "log_volume",
      "log_weight",
      "log_evidence",
      "log_evidence.var",
      "information"
    )
  )
})

test_that("clear method resets sampler state", {
  sampler_new <- ernest_sampler$new(
    log_lik_fn = gaussian_2$log_lik,
    prior = prior,
    sampler = rwmh_cube(),
    n_points = 500,
    first_update = 200L,
    update_interval = 50L
  )
  sampler$clear()
  expect_equal(as.list(sampler_new), as.list(sampler))
})
