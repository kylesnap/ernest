describe("new_ernest_prior", {
  fn <- function(x) x

  it("Explains dimensionality errors", {
    expect_snapshot(new_ernest_prior(fn, n_dim = 0), error = TRUE)
    expect_snapshot(
      new_ernest_prior(fn, n_dim = 3, lower = c(-Inf, 0)),
      error = TRUE
    )
    expect_snapshot(
      new_ernest_prior(
        fn,
        lower = c(-Inf, -Inf, 2),
        upper = c(Inf, Inf)
      ),
      error = TRUE
    )
    expect_snapshot_error(new_ernest_prior(
      fn,
      names = LETTERS[1:4],
      n_dim = 2
    ))
  })

  it("explains type errors", {
    expect_snapshot(new_ernest_prior(fn, lower = c("0", "0")), error = TRUE)
    expect_snapshot(new_ernest_prior(fn, names = c(60, 70)), error = TRUE)
  })

  it("explains errors with upper and lower", {
    expect_snapshot(new_ernest_prior(fn, lower = c(Inf, 0)), error = TRUE)
    expect_snapshot(new_ernest_prior(fn, upper = c(0, -Inf)), error = TRUE)
    expect_snapshot(new_ernest_prior(fn, lower = 0, upper = 0), error = TRUE)
  })

  prior <- NULL
  it("returns correct structure and class", {
    prior <<- new_ernest_prior(
      fn = fn,
      n_dim = 2,
      lower = c(0, 1),
      upper = c(1, 2),
      names = c("a", "b")
    )
    expect_type(prior, "list")
    expect_s3_class(prior, "ernest_prior")
    expect_equal(attr(prior, "n_dim"), 2)
    expect_equal(prior$lower, c(0, 1))
    expect_equal(prior$upper, c(1, 2))
    expect_equal(prior$names, c("a", "b"))
    expect_true(is.function(prior$fn))
  })

  it("can infer n_dim", {
    expect_identical(
      prior,
      new_ernest_prior(
        fn = fn,
        lower = c(0, 1),
        upper = c(1, 2),
        names = c("a", "b")
      )
    )
  })
})

describe("create_prior", {
  set.seed(42)
  it("creates a custom prior", {
    # 3D uniform prior in [-10, 10]
    unif <- function(x) {
      -10 + x * 20
    }
    # Single vector input
    prior <- create_prior(unif, lower = -10, upper = 10, .n_dim = 3)
    expect_equal(prior$fn(c(0.25, 0.5, 0.75)), c(-5, 0, 5))
    expect_snapshot(prior)
  })

  it("catchers issues with prior function output length", {
    fn <- function(x) c(x[1], x[2], x[1] / x[2])
    expect_error(
      create_prior(fn, .n_dim = 2),
      "`fn` must return a vector of length 2"
    )
  })

  it("errors if prior returns non-finite values", {
    fn <- function(x) rep(NaN, length(x))
    expect_error(
      create_prior(fn, .n_dim = 2),
      "`fn` must return vectors that only contain finite values"
    )
    fn <- function(x) c(x[1], NA)
    expect_error(
      create_prior(fn, .n_dim = 2),
      "`fn` must return vectors that only contain finite values"
    )
    fn <- function(x) c(Inf, x[2])
    expect_error(
      create_prior(fn, .n_dim = 2),
      "`fn` must return vectors that only contain finite values"
    )
  })

  it("errors if prior returns OOB values", {
    fn <- function(x) x
    expect_error(
      create_prior(fn, lower = 0.5, .n_dim = 2),
      "`fn` must respect the `lower` bounds"
    )
    expect_error(
      create_prior(fn, upper = 0.5, .n_dim = 2),
      "`fn` must respect the `upper` bounds"
    )
  })
})

test_that("new_ernest_prior repairs names as specified", {
  fn <- function(x) x
  prior <- create_prior(fn = fn, names = c("x", "x"))
  expect_equal(prior$names, c("x...1", "x...2"))

  expect_error(
    create_prior(fn = fn, names = c("x", "x"), .name_repair = "check_unique"),
    "Names must be unique"
  )
})
