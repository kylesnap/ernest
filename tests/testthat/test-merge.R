expect_most_mapequal <- function(object, expected, exclude = NULL) {
  obj_lst <- as.list(object)
  exp_lst <- as.list(expected)
  comp_names <- setdiff(
    c("unit", "log_lik", "nlive", "id", "neval", "birth_lik"),
    exclude
  )
  expect_mapequal(
    obj_lst[comp_names],
    exp_lst[comp_names]
  )
}

test_that("merging runs works", {
  run1 <- example_run
  run1$rcrd <- run1$rcrd[field(run1$rcrd, "id") <= 500L]
  run1$nlive <- 500

  run2 <- example_run
  run2$rcrd <- run2$rcrd[field(run2$rcrd, "id") > 500L]
  run2$nlive <- 500

  merged <- merge(run1, run2)
  expect_equal(merged$nlive, 1000)
  expect_most_mapequal(merged$rcrd, example_run$rcrd, exclude = "id")
})

describe("merge_rcrd", {
  expect_merged_ids <- function(object, x_nlive, y_nlive) {
    nlive <- x_nlive + y_nlive
    expect_equal(object$group_id$id[[1]], seq(x_nlive))
    expect_equal(object$group_id$id[[2]], seq(x_nlive + 1, nlive))
  }

  m <- NULL
  it("merges a run split by `id`", {
    rcrd <- example_run$rcrd
    first_half <- rcrd[field(rcrd, "id") <= 500L]
    second_half <- rcrd[field(rcrd, "id") > 500L]

    m <- merge_rcrd(first_half, second_half, "first")
    expect_merged_ids(m, 500, 500)
    expect_most_mapequal(m$rcrd, rcrd, exclude = "id")

    m <- merge_rcrd(first_half, second_half, "all")
    expect_merged_ids(m, 500, 500)
    expect_most_mapequal(m$rcrd, rcrd, exclude = "id")
  })

  it("merges a long run with a short run", {
    rcrd <- example_run$rcrd
    first_half <- rcrd[field(rcrd, "id") <= 500L]
    second_half <- rcrd[field(rcrd, "id") > 500L]
    second_half_id_loc <- vapply(
      vctrs::vec_group_loc(field(second_half, "id"))$loc,
      \(x) x[[1]],
      integer(1)
    )
    second_half <- second_half[second_half_id_loc]

    first_points_loc <- vapply(
      vctrs::vec_group_loc(field(rcrd, "id"))$loc,
      \(x) x[[1]],
      integer(1)
    )
    rcrd_first <- rcrd[first_points_loc]
    m <- merge_rcrd(first_half, second_half, "first")
    expect_merged_ids(m, 500, 500)
    expect_most_mapequal(
      m$rcrd,
      rcrd_first,
      exclude = c("id", "neval", "nlive")
    )
    expect_equal(field(m$rcrd, "nlive"), seq(1000, 1))
    expect_all_equal(field(m$rcrd, "neval"), 0L)

    m <- merge_rcrd(first_half, second_half, "all")
    expect_merged_ids(m, 500, 500)
    expect_equal(vctrs::vec_size(m$rcrd), length(first_half) + 500)

    # All history preserved from first run
    first_run_ids <- m$group_id$id[[1]]
    m_first_runs <- m$rcrd[field(m$rcrd, "id") %in% first_run_ids]
    expect_equal(field(m_first_runs, "log_lik"), field(first_half, "log_lik"))
    expect_equal(field(m_first_runs, "neval"), field(first_half, "neval"))
  })
})
