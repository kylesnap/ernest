fn <- \(x) {
  stats::qnorm(x, mean = c(-1, 0, 1))
}

test_that("create_prior throws errors", {
  expect_error(
    create_prior("fn"),
    "object 'fn' of mode 'function' was not found"
  )
})

describe("check_prior reports informative errors", {
  it("covers non-doubles", {
    bad_fn <- function(x) as.character(x)
    expect_error(
      check_prior(bad_fn, n_dim = 3, arg = "bad_fn(x)"),
      "Can't convert `bad_fn\\(x\\)` <character> to <double\\[,3\\]>."
    )
  })

  it("covers non-expected lengths", {
    short_fn <- function(x) {
      if (!is.matrix(x)) {
        x[1:2]
      } else {
        t(apply(x, 1, \(y) y[1:2]))
      }
    }
    expect_error(
      check_prior(short_fn, n_dim = 3, arg = "short_fn(x)"),
      "`short_fn\\(x\\)` must have 3 columns, not 2."
    )

    long_fn <- function(x) {
      if (!is.matrix(x)) {
        c(x[1:2], x[1])
      } else {
        t(apply(x, 1, \(y) c(y[1:2], y[1])))
      }
    }
    expect_error(
      check_prior(long_fn, n_dim = 2, arg = "long_fn(x)"),
      "`long_fn\\(x\\)` must have 2 columns, not 3."
    )
  })

  it("covers integers", {
    integer_fn <- \(x) {
      matrix(
        as.integer(stats::qnorm(p = c(x), mean = c(-1, 0, 1))),
        nrow = length(x) %/% 3,
        byrow = TRUE
      )
    }
    expect_error(
      check_prior(integer_fn, n_dim = 3, arg = "integer_fn(x)"),
      "`integer_fn\\(x\\)` must be a double matrix, not an integer matrix."
    )
  })

  it("covers non-finite values", {
    nonfinite_fn <- function(val) {
      function(x) {
        if (!is.matrix(x)) {
          c(x[1:2], val)
        } else {
          t(apply(x, 1, \(y) c(y[1:2], val)))
        }
      }
    }

    na_fn <- nonfinite_fn(NA)
    expect_error(
      check_prior(na_fn, n_dim = 3, arg = "na_fn(x)"),
      "`na_fn\\(x\\)` must contain no nonfinite values."
    )

    inf_fn <- nonfinite_fn(Inf)
    expect_error(
      check_prior(na_fn, n_dim = 3, arg = "inf_fn(x)"),
      "`inf_fn\\(x\\)` must contain no nonfinite values."
    )

    nan_fn <- nonfinite_fn(NaN)
    expect_error(
      check_prior(nan_fn, n_dim = 3, arg = "nan_fn(x)"),
      "`nan_fn\\(x\\)` must contain no nonfinite values."
    )
  })

  it("covers errors with explicit upper and lower", {
    expect_error(
      create_prior(fn, names = LETTERS[1:3], lower = c(Inf, 0)),
      "Can't recycle `lower` \\(size 2\\) to size 3."
    )
    expect_error(
      create_prior(fn, names = LETTERS[1:3], upper = c(0, -Inf)),
      "Can't recycle `upper` \\(size 2\\) to size 3."
    )
    expect_error(
      create_prior(fn, names = LETTERS[1:3], lower = 0, upper = 0),
      "`lower` bounds must be strictly smaller than `upper`."
    )
  })
})

test_that("fn is wrapped correctly", {
  test_matrix <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), nrow = 2)
  expected_prior <- t(apply(test_matrix, 1, fn))
  pr <- create_prior(fn, names = c("a", "b", "c"))

  expect_s3_class(pr, "ernest_prior")
  expect_equal(pr$names, c("a", "b", "c"))
  expect_equal(pr$fn(test_matrix[1, ]), expected_prior[1, ])
  expect_equal(pr$fn(test_matrix), expected_prior)
  expect_snapshot(pr)
})

describe("+.ernest_prior", {
  it("Can merge priors", {
    unif <- function(x) {
      -10 + x * 20
    }
    normal <- function(x) {
      stats::qnorm(x, mean = 1)
    }

    unif_p <- new_ernest_prior(unif, names = LETTERS[1:2])
    norm_p <- new_ernest_prior(normal, names = LETTERS[1:2])
    combo_p <- (unif_p + norm_p)

    expect_type(combo_p, "list")
    expect_s3_class(combo_p, "ernest_prior")
    expect_equal(attr(combo_p, "n_dim"), 4)
    expect_equal(combo_p$lower, c(-10, -10, -Inf, -Inf))
    expect_equal(combo_p$names, c("A...1", "B...2", "A...3", "B...4"))

    expect_equal(combo_p$fn(c(0.5, 0.5, 0.5, 0.5)), c(0, 0, 1, 1))
    combo_p2 <- (combo_p + unif_p)
    expect_equal(
      combo_p2$names,
      c("A...1", "B...2", "A...3", "B...4", "A...5", "B...6")
    )
    expect_equal(
      combo_p2$fn(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)),
      c(0, 0, 1, 1, 0, 0)
    )
  })

  it("Errors when concatenated with non-prior", {
    unif <- function(x) {
      -10 + x * 20
    }
    unif_p <- new_ernest_prior(unif, names = LETTERS[1:2])
    expect_error(
      unif_p + qnorm,
      "`y` must be an object with class ernest_prior, not a function."
    )
  })
})
