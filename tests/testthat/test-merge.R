data(example_run)

#' Helper function to compare two `ernest_rcrd` objects.
expect_rcrd_fuzzyequal <- function(object, expected, exclude = NULL) {
  obj_lst <- as.list(object)
  exp_lst <- as.list(expected)
  comp_names <- setdiff(
    c("unit", "log_lik", "nlive", "id", "neval", "birth_lik"),
    exclude
  )
  expect_mapequal(obj_lst[comp_names], exp_lst[comp_names])
}

#' Helper function to subset an `ernest_rcrd` by a set of live point IDs.
split_run <- function(rcrd, keep_ids) {
  rcrd[field(rcrd, "id") %in% keep_ids]
}

test_that("merge() supports `keep == first`", {
  run1 <- example_run
  run1$rcrd <- split_run(run1$rcrd, seq_len(500L))
  run1$nlive <- 500L

  run2 <- example_run
  run2$rcrd <- split_run(run2$rcrd, seq(501L, 1000L))
  run2$nlive <- 500L

  merged <- merge(run1, run2)
  expect_s3_class(merged, c("ernest_run", "ernest_sampler"))
  expect_equal(merged$nlive, 1000L)
  expect_rcrd_fuzzyequal(merged$rcrd, example_run$rcrd, exclude = "id")
})

test_that("merge() supports `keep = all`", {
  run1 <- example_run
  run1$rcrd <- split_run(run1$rcrd, seq_len(500L))
  run1$nlive <- 500L

  run2 <- example_run
  run2$rcrd <- split_run(run2$rcrd, seq(501L, 1000L))
  run2$nlive <- 500L

  merged <- merge(run1, run2, .keep = "all")
  expect_s3_class(merged, c("ernest_run", "ernest_sampler"))
  expect_equal(merged$nlive, 1000L)
  expect_rcrd_fuzzyequal(merged$rcrd, example_run$rcrd, exclude = "id")
})

describe("merge()", {
  it("fails when `y$rcrd` is not an `ernest_run`", {
    run1 <- example_run
    run2 <- "example_run"

    expect_error(
      merge(run1, y = run2),
      "must be an object with class ernest_run"
    )
    expect_error(
      merge(run1, run2),
      "must be an object with class ernest_run"
    )
  })

  it("fails when merged runs have different shapes", {
    run1 <- example_run
    run1$rcrd <- split_run(run1$rcrd, seq_len(500L))
    run1$nlive <- 500L

    run2 <- example_run
    run2$rcrd <- split_run(run2$rcrd, seq(501L, 1000L))
    attr(run2$rcrd, "nvar") <- 2L
    run2$nlive <- 500L

    expect_error(merge(run1, run2), "must have the same number of variables")
  })

  it("can merge a run split in three", {
    run1 <- example_run
    run1$rcrd <- split_run(run1$rcrd, seq_len(333L))
    run1$nlive <- 333L

    run2 <- example_run
    run2$rcrd <- split_run(run2$rcrd, seq(334L, 666L))
    run2$nlive <- 333L

    run3 <- example_run
    run3$rcrd <- split_run(run3$rcrd, seq(667L, 1000L))
    run3$nlive <- 334L

    merged <- merge(merge(run1, run2), run3)
    merged <- merge(run1, run2, run3)
    expect_equal(merged$nlive, 1000L)
    expect_rcrd_fuzzyequal(merged$rcrd, example_run$rcrd, exclude = "id")
  })
})

describe("merge_rcrd", {
  #' Get the first index of each ID in an `ernest_rcrd`.
  first_points <- \(x) {
    vapply(
      vctrs::vec_group_loc(field(x, "id"))$loc,
      \(x) x[[1]],
      integer(1)
    )
  }

  it("discards points if `keep == first`", {
    rcrd <- example_run$rcrd
    first_half <- split_run(rcrd, seq_len(500L))
    second_half <- split_run(rcrd, seq(501L, 1000L))
    second_half <- second_half[first_points(second_half)]
    first_run <- rcrd[first_points(rcrd)]

    merged_first <- merge_rcrd(first_half, second_half, "first")
    expect_length(merged_first, 1000L)
    expect_rcrd_fuzzyequal(
      merged_first,
      first_run,
      exclude = c("id", "neval", "nlive")
    )
    expect_all_equal(field(merged_first, "neval"), 0L)
  })

  it("preserves points if `keep = all`", {
    rcrd <- example_run$rcrd
    first_half <- split_run(rcrd, seq_len(500L))
    second_half <- split_run(rcrd, seq(501L, 1000L))
    second_half <- second_half[first_points(second_half)]

    merged_all <- merge_rcrd(first_half, second_half, "all")
    expect_length(merged_all, length(first_half) + length(second_half))

    first_run_ids <- seq_len(500L)
    merged_first_run <- merged_all[field(merged_all, "id") %in% first_run_ids]
    expect_equal(
      field(merged_first_run, "log_lik"),
      field(first_half, "log_lik")
    )
    expect_equal(field(merged_first_run, "neval"), field(first_half, "neval"))
  })
})
