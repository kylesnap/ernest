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
  res_rcrd <- res$rcrd

  expect_s3_class(res, c("ernest_run", "ernest_sampler"))
  expect_equal(res$nlive, 100L)
  expect_identical(sort(unique(vctrs::field(res_rcrd, "id"))), seq(100))
  expect_equal(length(res$rcrd), res$niter + 100)
  expect_equal(sum(vctrs::field(res_rcrd, "evals") == 0L), 100)
})

test_that("Parallel generate on an ernest_run", {
  data(example_run)
  prev_dead <- example_run$niter - example_run$nlive
  res <- generate(example_run, min_logz = 0.01, allow_par = TRUE)

  expect_equal(res$nlive, 1000L)
  expect_identical(sort(unique(vctrs::field(res$rcrd, "id"))), seq(1000))
  expect_equal(length(res$rcrd), res$niter + 1000)
  expect_equal(sum(vctrs::field(res$rcrd, "evals") == 0L), 1000)

  expect_gt(res$niter, example_run$niter)
  expect_gte(res$neval, example_run$neval)
  expect_identical(res$rcrd[1:prev_dead], example_run$rcrd[1:prev_dead])
})

mirai::daemons(0)
Sys.sleep(1)
