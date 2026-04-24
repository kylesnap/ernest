describe("get_insertion_indices", {
  make_faux_run <- function(df) {
    log_lik <- dnorm(df$unit, log = TRUE)
    birth_lik <- split(log_lik, df$id) |>
      lapply(\(x) c(-Inf, x[-length(x)])) |>
      unsplit(df$id)
    birth_iter <- split(seq_len(nrow(df)), df$id) |>
      lapply(\(x) c(0, x[-length(x)])) |>
      unsplit(df$id)
    evals <- split(df$id, df$id) |>
      lapply(\(x) c(rep(1, length(x) - 1), 0)) |>
      unsplit(df$id)
    res <- data_frame0(
      unit = df$unit,
      nlive = rep(NA_integer_, length(df$unit)),
      log_lik = log_lik,
      id = df$id,
      birth_lik = birth_lik,
      evals = evals,
      .birth_iter = birth_iter
    )
    # Slow loop to find the rank of each point.
    insertion <- integer(nrow(res))
    for (i in seq_len(nrow(res))) {
      log_lik <- res$log_lik[[i]]
      id <- res$id[[i]]
      birth <- res$birth_lik[[i]]
      if (birth == -Inf) {
        insertion[[i]] <- id
        next
      }
      f_res <- res[res$id != id & res$log_lik > birth, ]
      others <- vapply(
        split(f_res$log_lik, f_res$id),
        \(x) {
          min(x)
        },
        double(1)
      )
      insertion[[i]] <- rank(c(log_lik, others), ties.method = "first")[1]
    }
    res$.insertion <- insertion
    res
  }
  make_faux_rcrd <- function(df) {
    new_ernest_rcrd(
      unit = matrix(df$unit, ncol = 1),
      log_lik = df$log_lik,
      nlive = df$nlive,
      id = df$id,
      birth_lik = df$birth_lik,
      evals = df$evals
    )
  }

  get_insertion_indices(example_run$rcrd)
  it("computes insertion ranks for a simple run", {
    df <- data_frame0(
      unit = c(12:1),
      id = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
    ) |>
      make_faux_run()
    indices <- get_insertion_indices(make_faux_rcrd(df))

    expect_equal(indices$iter, df$.birth_iter)
    expect_equal(indices$id, df$id)
    expect_equal(indices$insertion, df$.insertion)
  })

  it("computes insertion ranks for a run with interleaved insertions", {
    df <- data_frame0(
      unit = c(12:1),
      id = c(1, 2, 1, 3, 2, 1, 1, 2, 3, 3, 2, 1)
    ) |>
      make_faux_run()

    indices <- get_insertion_indices(make_faux_rcrd(df))
    expect_equal(indices$iter, df$.birth_iter)
    expect_equal(indices$id, df$id)
    expect_equal(indices$insertion, df$.insertion)
  })
})

test_that("cusum plot works", {
  withr::local_seed(42)
  vdiffr::expect_doppelganger(
    "cusum example",
    calculate_cusum(example_run)
  )
})
