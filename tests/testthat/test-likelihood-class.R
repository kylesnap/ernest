#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages

# https://bnaras.github.io/cubature/articles/cubature.html#multivariate-normal
m <- 3
mean <- rep(0, m)
sigma <- diag(3)
sigma[2, 1] <- sigma[1, 2] <- 3 / 5
sigma[3, 1] <- sigma[1, 3] <- 1 / 3
sigma[3, 2] <- sigma[2, 3] <- 11 / 15
logdet <- sum(log(eigen(sigma, symmetric = TRUE, only.values = TRUE)$values))
fn <- function(x) {
  x <- matrix(x, ncol = length(x))
  distval <- stats::mahalanobis(x, center = mean, cov = sigma)
  exp(-(3 * log(2 * pi) + logdet + distval) / 2)
}

matrix_fn <- function(x) {
  distval <- stats::mahalanobis(x, center = mean, cov = sigma)
  exp(matrix(-(3 * log(2 * pi) + logdet + distval) / 2, nrow = nrow(x)))
}

test_that("create_likelihood throws errors", {
  expect_error(
    create_likelihood("fn"),
    "`fn` must be a function, not the string"
  )
  expect_error(
    create_likelihood(fn, on_nonfinite = "blob"),
    '`on_nonfinite` must be one of "warn", "quiet", or "abort"'
  )
  expect_error(
    create_likelihood(fn, vectorized_fn = matrix_fn),
    "Exactly one of `scalar_fn` or `vectorized_fn` must be supplied."
  )
})

describe("ernest_likelihood", {
  test_matrix <- matrix(c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), nrow = 2)
  expected_ll <- matrix_fn(test_matrix)

  ll <- create_likelihood(fn)
  it("produces scalar likelihoods", {
    expect_s3_class(ll, c("ernest_likelihood", "function"))
    expect_equal(attr(ll, "interface"), "scalar_fn")
    expect_equal(ll(c(0.0, 0.2, 0.4)), expected_ll[1])
    expect_equal(ll(test_matrix), drop(expected_ll))
    expect_snapshot(ll)
  })

  mat_ll <- create_likelihood(vectorized_fn = matrix_fn)
  it("produces likelihood from `vectorized_fn`", {
    expect_s3_class(mat_ll, c("ernest_likelihood", "function"))
    expect_equal(attr(mat_ll, "interface"), "vectorized_fn")
    expect_equal(mat_ll(c(0.0, 0.2, 0.4)), expected_ll[1])
    expect_equal(mat_ll(test_matrix), drop(expected_ll))
    expect_snapshot(mat_ll)
  })

  it("can be reconstructed", {
    ll2 <- create_likelihood(ll)
    expect_identical(ll2, ll)

    mat_ll2 <- create_likelihood(vectorized_fn = mat_ll)
    expect_identical(mat_ll2, mat_ll)
  })
})

describe("create_likelihood is controlled by  `on_finite`", {
  bad_vec <- c(1, 2, NaN)
  bad_mat <- matrix(c(1, 2, NaN, 1, 2, 3), ncol = 3, byrow = TRUE)

  it("fails when requested", {
    fail_ll <- create_likelihood(fn, on_nonfinite = "abort")
    fail_mat_ll <- create_likelihood(
      vectorized_fn = matrix_fn,
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
    warn_mat_ll <- create_likelihood(vectorized_fn = matrix_fn)
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
      vectorized_fn = matrix_fn,
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
    mat_ll <- create_likelihood(vectorized_fn = char_mat_fn)
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
    warn_mat_ll <- create_likelihood(vectorized_fn = matrix_fn)
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
      y[y > 0.01] <- Inf
      y
    }
    inf_mat_fn <- \(x) {
      y <- matrix_fn(x)
      y[y > 0.01] <- Inf
      y
    }

    bad_vec <- c(1, 2, 1)
    bad_mat <- matrix(c(1, 2, 1, 1, 2, 3), ncol = 3, byrow = TRUE)
    warn_ll <- create_likelihood(inf_fn)
    warn_mat_ll <- create_likelihood(vectorized_fn = inf_mat_fn)
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
