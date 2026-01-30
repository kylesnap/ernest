fn <- \(x) {
  stats::qnorm(x, mean = c(-1, 0, 1))
}
matrix_fn <- \(x) {
  y <- stats::qnorm(c(x), mean = rep(c(-1, 0, 1), each = nrow(x) %||% 1))
  dim(y) <- dim(x)
  y
}

test_that("create_prior throws errors", {
  expect_error(
    create_prior("fn"),
    "`fn` must be a function, not the string"
  )
  expect_error(
    create_prior(fn, vectorized_fn = matrix_fn),
    "Exactly one of `point_fn` or `vectorized_fn` must be supplied."
  )
})

describe("ernest_prior", {
  test_matrix <- matrix(c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), nrow = 2)
  expected_points <- matrix_fn(test_matrix)

  pr <- create_prior(fn, names = LETTERS[1:3])
  it("produces scalar likelihoods", {
    expect_s3_class(pr, c("custom_prior", "prior"))
    expect_equal(pr$names, c("A", "B", "C"))
    expect_equal(attr(pr, "interface"), "point_fn")
    expect_equal(pr$fn(c(0.0, 0.2, 0.4)), expected_points[1, , drop = FALSE])
    expect_equal(pr$fn(test_matrix), expected_points)
    expect_snapshot(pr)
  })

  mat_pr <- create_prior(vectorized_fn = matrix_fn, names = LETTERS[1:3])
  it("produces prior from `vectorized_fn`", {
    expect_s3_class(mat_pr, c("custom_prior", "prior"))
    expect_equal(pr$names, c("A", "B", "C"))
    expect_equal(attr(mat_pr, "interface"), "vectorized_fn")
    expect_equal(
      mat_pr$fn(c(0.0, 0.2, 0.4)),
      expected_points[1, , drop = FALSE]
    )
    expect_equal(mat_pr$fn(test_matrix), expected_points)
    expect_snapshot(mat_pr)
  })
})

describe("create_prior/check_prior", {
  matrix_wrap <- function(fn) {
    force(fn)
    \(x) {
      if (!is.matrix(x)) {
        dim(x) <- c(1, length(x))
      }
      apply(x, 1, fn)
    }
  }

  it("catches non-doubles", {
    bad_fn <- function(x) as.character(x)
    msg <- "<character\\[,3\\]> to <double\\[,3\\]>"
    expect_error(create_prior(bad_fn, names = LETTERS[1:3]), msg)
    expect_error(create_prior(matrix_wrap(bad_fn), names = LETTERS[1:3]), msg)
  })

  it("catches non-expected lengths", {
    short_fn <- \(x) x[1:2]
    msg <- "<double\\[,2\\]> to <double\\[,3\\]>"
    expect_error(create_prior(short_fn, names = LETTERS[1:3]), msg)
    expect_error(create_prior(matrix_wrap(short_fn), names = LETTERS[1:3]), msg)

    long_fn <- \(x) c(x, x[1])
    msg <- "<double\\[,3\\]> to <double\\[,2\\]>"
    expect_error(p <- create_prior(long_fn, names = LETTERS[1:2]), msg)
    expect_error(create_prior(matrix_wrap(long_fn), names = LETTERS[1:2]), msg)
  })

  it("converts integers into doubles", {
    int_fn <- \(x) as.integer(stats::qnorm(x), mean = c(-1, 0, 1))
    expect_no_message(p <- create_prior(int_fn, names = LETTERS[1:3]))
    expect_no_message(create_prior(matrix_wrap(int_fn), names = LETTERS[1:3]))
    expect_type(p$fn(c(0.5, 0.5, 0.5)), "double")
  })

  it("catches non-finite values", {
    nonfinite_fn <- function(nonfinite_val) {
      force(nonfinite_val)
      \(x) c(x[1:2], sample(c(x[3], nonfinite_val), 1, prob = c(0.9, 0.1)))
    }
    msg <- "must return only finite values."

    it("catches NA", {
      na_fn <- nonfinite_fn(NA)
      expect_error(create_prior(na_fn, names = LETTERS[1:3]), msg)
      expect_error(create_prior(matrix_wrap(na_fn), names = LETTERS[1:3]), msg)
    })

    it("catches Inf", {
      inf_fn <- nonfinite_fn(Inf)
      expect_error(create_prior(inf_fn, names = LETTERS[1:3]), msg)
      expect_error(create_prior(matrix_wrap(inf_fn), names = LETTERS[1:3]), msg)
    })

    it("catches NaN", {
      nan_fn <- nonfinite_fn(NaN)
      expect_error(create_prior(nan_fn, names = LETTERS[1:3]), msg)
      expect_error(create_prior(matrix_wrap(nan_fn), names = LETTERS[1:3]), msg)
    })
  })

  it("detects errors with explicit upper and lower", {
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
    expect_error(
      create_prior(fn, names = LETTERS[1:3], lower = c(0, -Inf, -Inf)),
      "must respect the lower bounds."
    )
    expect_error(
      create_prior(fn, names = LETTERS[1:3], upper = 1),
      "must respect the upper bounds."
    )
  })
})

describe("+.ernest_prior", {
  it("Can merge priors", {
    unif <- function(x) {
      -10 + x * 20
    }
    normal <- function(x) {
      stats::qnorm(x, mean = 1)
    }

    unif_p <- create_prior(
      unif,
      names = LETTERS[1:2],
      lower = c(-10, -10),
      upper = c(10, 10)
    )
    norm_p <- create_prior(normal, names = LETTERS[1:2])
    combo_p <- (unif_p + norm_p)

    expect_type(combo_p, "list")
    expect_s3_class(combo_p, "ernest_prior")
    expect_equal(attr(combo_p, "n_dim"), 4)
    expect_equal(combo_p$lower, c(-10, -10, -Inf, -Inf))
    expect_equal(combo_p$names, c("A...1", "B...2", "A...3", "B...4"))

    expect_equal(
      combo_p$fn(c(0.5, 0.5, 0.5, 0.5)),
      matrix(c(0, 0, 1, 1), nrow = 1)
    )
    combo_p2 <- (combo_p + unif_p)
    expect_equal(
      combo_p2$names,
      c("A...1", "B...2", "A...3", "B...4", "A...5", "B...6")
    )
    expect_equal(
      combo_p2$fn(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)),
      matrix(c(0, 0, 1, 1, 0, 0), nrow = 1)
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
