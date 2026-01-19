#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages

fn <- \(x) sum(stats::dnorm(x, mean = c(-1, 0, 1), log = TRUE))
matrix_fn <- \(x) mvtnorm::dmvnorm(x, mean = c(-1, 0, 1), log = TRUE)

test_that("create_likelihood throws errors", {
  expect_snapshot(create_likelihood("fn"), error = TRUE)
  expect_snapshot(create_likelihood(fn, on_nonfinite = "blob"), error = TRUE)
  expect_snapshot(create_likelihood(fn, matrix_fn = matrix_fn), error = TRUE)
})

describe("matrix_fn and fn args are similar", {
  test_matrix <- matrix(c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), nrow = 2)
  expected_ll <- mvtnorm::dmvnorm(test_matrix, mean = c(-1, 0, 1), log = TRUE)

  ll <- create_likelihood(fn)
  it("produces likelihoods from `fn`", {
    expect_s3_class(ll, c("ernest_likelihood", "function"))
    expect_equal(attr(ll, "matrix_compat"), "auto")
    expect_equal(ll(c(0.0, 0.2, 0.4)), expected_ll[1])
    expect_equal(ll(test_matrix), expected_ll)
    expect_snapshot(ll)
  })

  mat_ll <- create_likelihood(matrix_fn = matrix_fn)
  it("produces likelihood from `matrix_fn`", {
    expect_s3_class(mat_ll, c("ernest_likelihood", "function"))
    expect_equal(attr(mat_ll, "matrix_compat"), "user")
    expect_equal(mat_ll(c(0.0, 0.2, 0.4)), expected_ll[1])
    expect_equal(mat_ll(test_matrix), expected_ll)
    expect_snapshot(mat_ll)
  })

  it("can be reconstructed", {
    ll2 <- create_likelihood(ll)
    expect_identical(ll2, ll)

    mat_ll2 <- create_likelihood(mat_ll)
    expect_identical(mat_ll2, mat_ll)
  })
})

describe("create_likelihood is controlled by  `on_finite`", {
  bad_vec <- c(1, 2, NaN)
  bad_mat <- matrix(c(1, 2, NaN, 1, 2, 3), ncol = 3, byrow = TRUE)

  it("fails when requested", {
    fail_ll <- create_likelihood(fn, on_nonfinite = "abort")
    fail_mat_ll <- create_likelihood(
      matrix_fn = matrix_fn,
      on_nonfinite = "abort"
    )
    expected_msg <- "Detected non-viable value: `NaN`"

    expect_error(fail_ll(bad_vec), expected_msg)
    expect_error(fail_mat_ll(bad_vec), expected_msg)
    expect_error(fail_ll(bad_mat), expected_msg)
    expect_error(fail_mat_ll(bad_mat), expected_msg)
  })

  it("warns on default", {
    warn_ll <- create_likelihood(fn, )
    warn_mat_ll <- create_likelihood(matrix_fn = matrix_fn)
    expected_msg <- "Replacing `NaN` with `-Inf`."
    expected_value <- c(-Inf, fn(c(1, 2, 3)))

    expect_warning(res <- warn_ll(bad_vec), expected_msg)
    expect_equal(res, -Inf)
    expect_warning(res <- warn_mat_ll(bad_vec), expected_msg)
    expect_equal(res, -Inf)
    expect_warning(res <- warn_ll(bad_mat), expected_msg)
    expect_equal(res, expected_value)
    expect_warning(warn_mat_ll(bad_mat), expected_msg)
    expect_equal(res, expected_value)
  })

  it("remains silent on request", {
    quiet_ll <- create_likelihood(fn, on_nonfinite = "quiet")
    quiet_mat_ll <- create_likelihood(
      matrix_fn = matrix_fn,
      on_nonfinite = "quiet"
    )
    expected_value <- c(-Inf, fn(c(1, 2, 3)))

    expect_no_message(res <- quiet_ll(bad_vec))
    expect_equal(res, -Inf)
    expect_no_message(res <- quiet_mat_ll(bad_vec))
    expect_equal(res, -Inf)
    expect_no_message(res <- quiet_ll(bad_mat))
    expect_equal(res, expected_value)
    expect_no_message(quiet_mat_ll(bad_mat))
    expect_equal(res, expected_value)
  })
})

describe("handles other nonfinite values", {
  it("characters", {
    char_fn <- \(x) as.character(fn(x))
    char_mat_fn <- \(x) as.character(matrix_fn(x))
    ll <- create_likelihood(char_fn, )
    mat_ll <- create_likelihood(matrix_fn = char_mat_fn)
    expect_error(
      ll(c(0, 1, 2)),
      "Can't convert `log_lik\\(x\\)` <character> to <double>."
    )
    expect_error(
      mat_ll(c(0, 1, 2)),
      "Can't convert `log_lik\\(x\\)` <character> to <double>."
    )
  })

  it("Missing values", {
    bad_vec <- c(1, 2, NA)
    bad_mat <- matrix(c(1, 2, NA, 1, 2, 3), ncol = 3, byrow = TRUE)
    warn_ll <- create_likelihood(fn)
    warn_mat_ll <- create_likelihood(matrix_fn = matrix_fn)
    expected_msg <- "Replacing `NA` with `-Inf`."
    expected_value <- c(-Inf, fn(c(1, 2, 3)))

    expect_warning(res <- warn_ll(bad_vec), expected_msg)
    expect_equal(res, -Inf)
    expect_warning(res <- warn_mat_ll(bad_vec), expected_msg)
    expect_equal(res, -Inf)
    expect_warning(res <- warn_ll(bad_mat), expected_msg)
    expect_equal(res, expected_value)
    expect_warning(warn_mat_ll(bad_mat), expected_msg)
    expect_equal(res, expected_value)
  })

  it("Positive infinite values", {
    inf_fn <- \(x) {
      y <- fn(x)
      y[y > -8] <- Inf
      y
    }
    inf_mat_fn <- \(x) {
      y <- matrix_fn(x)
      y[y > -8] <- Inf
      y
    }

    bad_vec <- c(1, 2, 1)
    bad_mat <- matrix(c(1, 2, 1, 1, 2, 3), ncol = 3, byrow = TRUE)
    warn_ll <- create_likelihood(inf_fn)
    warn_mat_ll <- create_likelihood(matrix_fn = inf_mat_fn)
    expected_msg <- "Replacing `Inf` with `-Inf`."
    expected_value <- c(-Inf, fn(c(1, 2, 3)))

    expect_warning(res <- warn_ll(bad_vec), expected_msg)
    expect_equal(res, -Inf)
    expect_warning(res <- warn_mat_ll(bad_vec), expected_msg)
    expect_equal(res, -Inf)
    expect_warning(res <- warn_ll(bad_mat), expected_msg)
    expect_equal(res, expected_value)
    expect_warning(warn_mat_ll(bad_mat), expected_msg)
    expect_equal(res, expected_value)
  })
})
