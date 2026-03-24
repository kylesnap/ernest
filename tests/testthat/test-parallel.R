test_that("thread_nlive correctly handles parallel workers", {
  mock_hint <- "MOCK MESSAGE"
  expect_error(
    thread_nlive(nlive = 100, workers = integer(0), hint = mock_hint),
    "At least one worker must be specified"
  )

  workers <- c(NA, NA, NA)
  result <- thread_nlive(nlive = 100, workers = workers, hint = mock_hint)
  expect_identical(result, c(34L, 33L, 33L))

  workers <- c(10, 0, 15)
  expect_error(
    thread_nlive(nlive = 100, workers = workers, hint = mock_hint),
    "parallel runs must each contain at least one live point"
  )

  workers <- c(10, 20, 15)
  result <- thread_nlive(nlive = 100, workers = workers, hint = mock_hint)
  expect_identical(result, c(65L, 20L, 15L))
})
skip("Under construction")

mirai::daemons(n = 2)
test_that("Parallel generate on an ernest_run", {
  sampler <- ernest_sampler(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior,
    seed = 42
  )
  res <- generate(sampler, max_iterations = 500, allow_par = TRUE)
  print(res)
})

test_that("Parallel generate on an ernest_run", {
  print(example_run)
  print(generate(example_run, min_logz = 0.01, allow_par = TRUE))
})
mirai::daemons(0)
