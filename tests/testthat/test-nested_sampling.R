test_that("nested_sampling function works correctly", {
  n_dim <- 2
  sampler <- rwmh_cube()
  n_points <- 500
  first_update <- 2.0
  between_update <- 1.5
  verbose <- FALSE

  result <- nested_sampling(
    gaussian_2$log_lik,
    gaussian_2$prior_transform,
    n_dim,
    sampler,
    n_points,
    first_update,
    between_update,
    verbose
  )

  expect_s3_class(result, "ErnestSampler")
  expect_mapequal(
    result,
    list(
      log_lik = gaussian_2$log_lik,
      prior_transform = gaussian_2$prior_transform,
      ptype = tibble::tibble(
        "X...1" = double(),
        "X...2" = double()
      ),
      n_dim = 2L,
      n_points = 500L,
      first_update = 1000L,
      between_update = 750L,
      verbose = FALSE,
      wrk = NULL,
      steps = 20,
      epsilon = 1.0,
      p_acc = 0.5
    )
  )
})

test_that("nested_sampling.default throws errors", {
  expect_error(nested_sampling(1), "No method defined for")
  log_lik <- gaussian_2$log_lik
  prior_trans <- gaussian_2$prior_transform
  expect_error(
    nested_sampling(log_lik, prior_trans, n_dim = "n_dim"),
    "must be a whole number"
  )
  expect_error(
    nested_sampling(log_lik, prior_trans, 2, n_points = "500"),
    "must be a whole number"
  )
  expect_error(
    nested_sampling(log_lik, prior_trans, 2, sampler = list()),
    "sampler must be an ernest_lrps object"
  )
})

test_that("Default parameters behave as expected", {
  result <- nested_sampling(
    gaussian_2$log_lik,
    gaussian_2$prior_transform,
    2
  )

  expect_s3_class(result, c("RandomWalkCube", "ErnestSampler"))
  expect_mapequal(
    result,
    list(
      log_lik = gaussian_2$log_lik,
      prior_transform = gaussian_2$prior_transform,
      ptype = tibble::tibble(
        "X...1" = double(),
        "X...2" = double()
      ),
      n_dim = 2L,
      n_points = 500L,
      first_update = 1000L,
      between_update = 750L,
      verbose = getOption("verbose"),
      wrk = NULL,
      steps = 20,
      epsilon = 1.0,
      p_acc = 0.5
    )
  )
})
