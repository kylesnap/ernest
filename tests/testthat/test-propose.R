test_that("propose_uniform.ernest_lrps works correctly", {
  x <- list(
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2
  )

  result <- propose_uniform.ernest_lrps(x, criterion = -Inf)

  expect_type(result, "list")
  expect_length(result$unit, x$n_dim)
  expect_length(result$parameter, x$n_dim)
  expect_true(is.numeric(result$log_lik))
  expect_true(is.numeric(result$num_calls))
})

test_that("propose_live.ernest_lrps works correctly", {
  x <- list(
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2
  )

  original <- c(0.5, 0.5)
  result <- propose_live.ernest_lrps(x, original, criterion = -Inf)

  expect_type(result, "list")
  expect_length(result$unit, x$n_dim)
  expect_length(result$parameter, x$n_dim)
  expect_true(is.numeric(result$log_lik))
  expect_true(is.numeric(result$num_calls))
})

test_that("propose_live.rw_cube works correctly", {
  x <- list(
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2,
    num_steps = 10L,
    epsilon = list(1, 0.5),
    args = new_environment()
  )

  original <- c(0.5, 0.5)
  result <- propose_live.rw_cube(x, original, criterion = -Inf)

  expect_type(result, "list")
  expect_length(result$unit, x$n_dim)
  expect_length(result$parameter, x$n_dim)
  expect_true(is.numeric(result$log_lik))
  expect_true(is.numeric(result$num_calls))
  expect_true(is.numeric(result$num_acc))
})

test_that("propose_uniform.ernest_lrps returns blank if the max cannot be reached", {
  x <- list(
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2
  )
  options(ernest.max_loop = 100)

  res <- propose_uniform.ernest_lrps(x, criterion = Inf)
  expect_equal(
    propose_uniform.ernest_lrps(x, criterion = Inf),
    list("num_calls" = 100)
  )
})

test_that("propose_live.rw_cube returns the original point if the max cannot be reached", {
  x <- list(
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2,
    num_steps = 10L,
    epsilon = list(1, 0.5),
    args = new_environment()
  )

  original <- c(0.5, 0.5)
  expect_equal(
    propose_live.rw_cube(x, original, criterion = Inf),
    list(
      "unit" = original,
      "parameter" = c(0, 0),
      "log_lik" = x$log_lik(c(0, 0)),
      "num_calls" = 10,
      "num_acc" = 0
    )
  )
})

test_that("propose_live.rw_cube is reproducible with set.seed", {
  set.seed(123)
  x <- list(
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2,
    num_steps = 10L,
    epsilon = list(1, 0.5),
    args = new_environment()
  )
  original <- c(0.5, 0.5)
  criterion <- -Inf

  result1 <- replicate(
    10,
    propose_live.rw_cube(x, original, criterion)
  )

  set.seed(123)
  result2 <- replicate(
    10,
    propose_live.rw_cube(x, original, criterion)
  )

  expect_equal(result1, result2)
})

test_that("propose_live.unif_cube is reproducible with set.seed", {
  set.seed(123)
  x <- list(
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2
  )
  original <- c(0.5, 0.5)
  criterion <- -Inf

  result1 <- replicate(
    10,
    propose_uniform.ernest_lrps(x, criterion)
  )

  set.seed(123)
  result2 <- replicate(
    10,
    propose_uniform.ernest_lrps(x, criterion)
  )

  expect_equal(result1, result2)
})
