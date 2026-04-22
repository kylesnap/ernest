describe("get_insertion_indices", {
  faux_rcrd <- function(log_lik, id, birth_lik) {
    new_ernest_rcrd(
      unit = matrix(0, nrow = length(log_lik), ncol = 1),
      log_lik = log_lik,
      nlive = rep(3L, length(log_lik)),
      id = id,
      birth_lik = birth_lik,
      evals = rep(0L, length(log_lik))
    )
  }

  it("computes insertion ranks for a simple run", {
    rcrd <- faux_rcrd(
      log_lik = c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1),
      id = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1),
      birth_lik = c(-Inf, -Inf, -Inf, -10, -9, -8, -7, -6, -5, -4)
    )
    insertions <- get_insertion_indices(rcrd)

    expect_equal(insertions$iter, c(NA, NA, NA, 1, 2, 3, 4, 5, 6, 7))
    expect_equal(insertions$id, c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1))
    expect_equal(insertions$insertion, c(1, 2, 3, 3, 3, 3, 3, 3, 3, 3))
  })

  it("computes insertion ranks for a run with interleaved insertions", {
    rcrd <- faux_rcrd(
      log_lik = c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1),
      id = c(1, 2, 3, 3, 2, 1, 3, 2, 1, 3),
      birth_lik = c(-Inf, -Inf, -Inf, -8, -9, -10, -7, -6, -5, -4)
    )
    insertions <- get_insertion_indices(rcrd)

    expect_equal(insertions$iter, c(NA, NA, NA, 3, 2, 1, 4, 5, 6, 7))
    expect_equal(insertions$id, c(1, 2, 3, 3, 2, 1, 3, 2, 1, 3))
    expect_equal(insertions$insertion, c(1, 2, 3, 1, 2, 3, 3, 3, 3, 3))
  })
})

test_that("crosschecks work and are repeatable", {
  withr::local_seed(42)
  vdiffr::expect_doppelganger(
    "crosscheck example",
    crosscheck_plot(example_run)
  )

  expect_snapshot(crosscheck_tests(example_run))
})
