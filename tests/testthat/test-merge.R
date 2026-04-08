run1 <- generate(
  ernest_sampler(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior,
    seed = 24
  ),
  max_iterations = 100
)
run2 <- generate(
  ernest_sampler(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior,
    nlive = 300,
    seed = 42
  ),
  max_iterations = 100
)

test_that("reindex_runs remaps IDs and preserves record fields", {
  x1 <- run1$rcrd
  x2 <- run2$rcrd
  out <- reindex_runs(x1, x2)

  n1 <- length(x1)
  n2 <- length(x2)
  nlive1 <- vctrs::vec_unique_count(vctrs::field(x1, "id"))
  nlive2 <- vctrs::vec_unique_count(vctrs::field(x2, "id"))

  expect_length(out, 2)
  expect_identical(sort(unique(vctrs::field(out[[1]], "id"))), seq(nlive1))
  expect_identical(
    sort(unique(vctrs::field(out[[2]], "id"))),
    seq(nlive1 + 1, nlive1 + nlive2)
  )

  expect_identical(
    vctrs::field(out[[1]], "log_lik"),
    vctrs::field(x1, "log_lik")
  )
  expect_identical(
    vctrs::field(out[[2]], "log_lik"),
    vctrs::field(x2, "log_lik")
  )
})

test_that("merge_results errors when IDs are duplicated across runs", {
  x1 <- run1$rcrd
  x2 <- run2$rcrd

  expect_error(
    merge_results(x1, x2),
    "must contain unique IDs"
  )
})

test_that("merge_results returns expected dead/live partition", {
  x1 <- run1$rcrd
  x2 <- run2$rcrd
  indexed <- reindex_runs(x1, x2)
  res <- merge_results(indexed[[1]], indexed[[2]])
  expect_named(res, c("live", "dead", "ndrop"))

  nlive_total <- 800L
  expect_equal(nrow(field(res$live, "unit")), nlive_total)
  expect_equal(
    length(res$dead) + length(res$live) + res$ndrop,
    length(x1) + length(x2)
  )
  expect_equal(
    min(field(res$live, "log_lik")),
    min(
      vctrs::field(run1$rcrd, "log_lik")[
        vctrs::field(run1$rcrd, "evals") == 0
      ],
      vctrs::field(run2$rcrd, "log_lik")[
        vctrs::field(run2$rcrd, "evals") == 0
      ]
    )
  )
  expect_all_equal(vctrs::field(res$live, "evals"), 0)
  expect_all_true(vctrs::field(res$dead, "evals") > 0L)
  expect_identical(vctrs::field(res$live, "id"), seq(nlive_total))
  expect_false(is.unsorted(vctrs::field(res$dead, "log_lik")))
})

test_that("merging two runs", {
  run3 <- merge(run1, run2)
  run3_rcrd <- run3$rcrd
  expect_identical(run3$nlive, 800L)
  expect_lte(run3$niter, 200L)
  expect_identical(sort(unique(vctrs::field(run3_rcrd, "id"))), seq(run3$nlive))
  expect_length(vctrs::field(run3_rcrd, "id"), run3$niter + 800)
  expect_equal(run3$first_update, 800 * 2.5)
  expect_equal(run3$update_interval, 800 * 1.5)
  expect_identical(attr(run3, "seed"), NA_integer_)

  run4 <- generate(run3, max_iterations = run3$niter + 100L, min_logz = 0)
  run4_rcrd <- run4$rcrd
  expect_equal(run4$niter, run3$niter + 100L)
  expect_gt(run4$neval, run3$neval)
  expect_identical(
    vctrs::field(run3_rcrd, "log_lik")[1:run3$niter],
    vctrs::field(run4_rcrd, "log_lik")[1:run3$niter]
  )
})
