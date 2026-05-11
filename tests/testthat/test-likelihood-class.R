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
    expect_error(ll(c("0", "0.2", "0.4")), "must be a numeric vector")
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

describe("handles type conversion errors", {
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
})

#' Missing value catching
#'
#' @srrstats {BS2.14} Tests whether warnings are surpressed upon request.
#' @srrstats {G5.3} Ernest results do not contain NA even when log-lik produces
#' NA values.
NULL

test_that("Missing values in the log-likelihood", {
  set.seed(42)
  ll_fn_missing <- \(theta) {
    if (all(theta >= 0)) {
      return(NA)
    }
    gaussian_blobs$log_lik(theta)
  }

  expect_snapshot(
    ernest_sampler(
      log_lik = create_likelihood(ll_fn_missing, on_nonfinite = "abort"),
      prior = gaussian_blobs$prior,
      seed = 42
    ),
    transform = \(x) gsub("\\d+\\.\\d+", "#\\.#", x),
    error = TRUE
  )

  expect_no_message(
    quiet_na_sampler <- ernest_sampler(
      create_likelihood(ll_fn_missing, on_nonfinite = "quiet"),
      gaussian_blobs$prior,
      seed = 42
    )
  )

  expect_snapshot(
    ernest_sampler(
      create_likelihood(ll_fn_missing, on_nonfinite = "warn"),
      gaussian_blobs$prior,
      seed = 42
    ),
    transform = \(x) gsub("\\d+\\.\\d+", "#\\.#", x)
  )

  run <- generate(quiet_na_sampler, max_iterations = 100L)
  expect_false(anyNA(field(run$rcrd, "log_lik")))
})
