data(example_run)
skip("CURRENTLY BROKEN")

#' Helper function to subset an `ernest_rcrd` by a set of live point IDs.
split_run <- function(rcrd, keep_ids) {
  rcrd[field(rcrd, "id") %in% as.character(keep_ids)]
}

#' Helper function to subset an `ernest_rcrd` by an iteration.
split_run_iter <- function(rcrd, indices) {
  rcrd <- vctrs::vec_slice(
    rcrd,
    vctrs::num_as_location(indices, n = length(rcrd))
  )
  ids <- vctrs::vec_group_loc(field(rcrd, "id"))
  nlive <- length(ids$key)
  last_idx <- vapply(ids$loc, \(i) i[[length(i)]], integer(1))
  vctrs::field(rcrd, "nlive") <- pmax(field(rcrd, "nlive"), nlive)
  vctrs::field(rcrd[last_idx], "neval") <- rep(0L, nlive)
  list("rcrd" = rcrd, "nlive" = nlive)
}

test_that("merging fails when arguments are poorly specified", {
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

  run2 <- example_run
  run2$rcrd <- split_run(run2$rcrd, seq(501L, 1000L))
  attr(run2$rcrd, "nvar") <- 2L
  run2$nlive <- 500L
  expect_error(merge(run1, run2), "`nvar` attribute must match")
})

describe("merge() recreates the example_run when split by `id`", {
  it("works when the run in split in half", {
    run1 <- example_run
    run1$rcrd <- split_run(run1$rcrd, seq_len(500L))
    run1$nlive <- 500L

    run2 <- example_run
    run2$rcrd <- split_run(run2$rcrd, seq(501L, 1000L))
    run2$nlive <- 500L

    merged <- merge(run1, run2)
    expect_s3_class(merged, c("ernest_run", "ernest_sampler"))
    expect_equal(merged$rcrd, example_run$rcrd)

    merged <- merge(run1, run2, keep = "all")
    expect_s3_class(merged, c("ernest_run", "ernest_sampler"))
    expect_equal(merged$rcrd, example_run$rcrd)
  })

  it("works when split in thirds", {
    run1 <- example_run
    run1$rcrd <- split_run(run1$rcrd, seq_len(333L))
    run1$nlive <- 333L

    run2 <- example_run
    run2$rcrd <- split_run(run2$rcrd, seq(334L, 666L))
    run2$nlive <- 333L

    run3 <- example_run
    run3$rcrd <- split_run(run3$rcrd, seq(667L, 1000L))
    run3$nlive <- 334L

    merged <- merge(run1, merge(run2, run3))
    expect_equal(merged$nlive, 1000L)
    expect_equal(merged$rcrd, example_run$rcrd)
  })
})

describe("merge() recreates the example_run when split by `iter`", {
  tot <- length(example_run$rcrd)
  bp <- tot %/% 2

  it("works when the run in split in half", {
    run1 <- example_run
    spl1 <- split_run_iter(run1$rcrd, seq_len(bp))
    run1$rcrd <- spl1$rcrd
    run1$nlive <- spl1$nlive

    run2 <- example_run
    spl2 <- split_run_iter(run2$rcrd, seq(bp + 1, tot))
    run2$rcrd <- spl2$rcrd
    run2$nlive <- spl2$nlive

    merged <- merge(run1, run2)
    expect_s3_class(merged, c("ernest_run", "ernest_sampler"))
    expect_setequal(
      field(merged$rcrd, "id"),
      c(
        paste0(field(spl1$rcrd, "id"), ".x"),
        paste0(field(spl2$rcrd, "id"), ".y")
      )
    )
    print(merged)

    merged <- merge(run1, run2, keep = "all")
    expect_setequal(
      field(merged$rcrd, "id"),
      c(
        paste0(field(spl1$rcrd, "id"), ".x"),
        paste0(field(spl2$rcrd, "id"), ".y")
      )
    )
    print(merged)
  })
})
