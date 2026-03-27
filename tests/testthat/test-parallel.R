test_that("thread_nlive correctly specifies parallel workers", {
  result <- thread_nlive(example_run, nworkers = 3)
  expect_length(result, 3)
  expect_length(result[[1]], 334)
  expect_length(result[[2]], 333)
  expect_length(result[[3]], 333)
})

test_that("Parallel generate fails when no daemons are set", {
  expect_error(
    generate(example_run, min_logz = 0.01, allow_par = TRUE),
    "No daemons set."
  )
})

mirai::daemons(1, dispatcher = FALSE)

test_that("Parallel generate on an ernest_sampler", {
  res <- expect_gaussian_run(rwmh_cube(), .generate = list(allow_par = TRUE))

  expect_s3_class(res, c("ernest_run", "ernest_sampler"))
  expect_equal(res$nlive, 100L)
  expect_identical(sort(unique(res$weights$id)), seq(100))
  expect_equal(nrow(res$samples$unit_cube), res$niter + 100)
  expect_equal(sum(res$weights$evaluations == 0L), 100)
})

test_that("Parallel generate on an ernest_run", {
  data(example_run)
  prev_dead <- example_run$niter - example_run$nlive
  res <- generate(example_run, min_logz = 0.01, allow_par = TRUE)

  expect_equal(res$nlive, 1000L)
  expect_identical(sort(unique(res$weights$id)), seq(1000))
  expect_equal(nrow(res$samples$unit_cube), res$niter + 1000)
  expect_equal(sum(res$weights$evaluations == 0L), 1000)

  expect_gt(res$niter, example_run$niter)
  expect_gte(res$neval, example_run$neval)
  expect_identical(
    example_run$samples$unit_cube[1:prev_dead, ],
    res$samples$unit_cube[1:prev_dead, ]
  )
  expect_identical(
    example_run$weights$log_lik[1:prev_dead],
    res$weights$log_lik[1:prev_dead]
  )
})

mirai::daemons(0)
Sys.sleep(1)
