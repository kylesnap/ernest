describe("new_ernest_prior", {
  fn <- function(x) x

  it("Explains dimensionality errors", {
    expect_snapshot(new_ernest_prior(fn), error = TRUE)
    expect_snapshot(
      new_ernest_prior(fn, LETTERS[1:3], lower = c(-Inf, 0)),
      error = TRUE
    )

    expect_snapshot(
      new_ernest_prior(
        fn,
        LETTERS[1:3],
        lower = c(-Inf, -Inf, 2),
        upper = c(Inf, Inf)
      ),
      error = TRUE
    )
  })

  it("explains type errors", {
    expect_snapshot(new_ernest_prior(fn = "mean"), error = TRUE)
    expect_snapshot(new_ernest_prior(fn, lower = c("0", "0")), error = TRUE)
  })

  it("explains errors with explicit upper and lower", {
    expect_snapshot(
      new_ernest_prior(fn, names = LETTERS[1:2], lower = c(Inf, 0)),
      error = TRUE
    )
    expect_snapshot(
      new_ernest_prior(fn, names = LETTERS[1:2], upper = c(0, -Inf)),
      error = TRUE
    )
    expect_snapshot(
      new_ernest_prior(fn, names = LETTERS[1:2], lower = 0, upper = 0),
      error = TRUE
    )
  })

  it("returns correct structure and class", {
    prior <- new_ernest_prior(
      fn = fn,
      names = c("a", "b"),
      lower = c(0, 0),
      upper = c(1, 1)
    )

    expect_type(prior, "list")
    expect_s3_class(prior, "ernest_prior")
    expect_equal(attr(prior, "n_dim"), 2)
    expect_equal(prior$lower, c(0, 0))
    expect_equal(prior$upper, c(1, 1.0))
    expect_equal(prior$names, c("a", "b"))
    expect_true(is.function(prior$fn))
  })
})

describe("c.ernest_prior", {
  it("Can merge priors", {
    unif <- function(x) {
      -10 + x * 20
    }
    normal <- function(x) {
      stats::qnorm(x, mean = 1)
    }

    unif_p <- new_ernest_prior(unif, names = LETTERS[1:2])
    norm_p <- new_ernest_prior(normal, names = LETTERS[1:2])
    combo_p <- c(unif_p, norm_p)

    expect_type(combo_p, "list")
    expect_s3_class(combo_p, "ernest_prior")
    expect_equal(attr(combo_p, "n_dim"), 4)
    expect_equal(combo_p$lower, c(-10, -10, -Inf, -Inf))
    expect_equal(combo_p$names, c("A...1", "B...2", "A...3", "B...4"))

    expect_equal(combo_p$fn(c(0.5, 0.5, 0.5, 0.5)), c(0, 0, 1, 1))

    combo_p2 <- c(combo_p, unif_p)
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
    expect_snapshot(c(unif_p, qnorm), error = TRUE)
  })

  it("Is idempotent", {
    unif <- function(x) {
      -10 + x * 20
    }
    unif_p <- new_ernest_prior(unif, names = LETTERS[1:2])
    expect_equal(c(unif_p), unif_p)
  })
})

describe("create_prior", {
  set.seed(42)
  it("creates a custom prior", {
    # 3D uniform prior in [-10, 10]
    unif <- function(x) {
      -10 + x * 20
    }
    prior <- create_prior(
      unif,
      LETTERS[1:2],
      lower = c(-10, -10),
      upper = c(10, 10)
    )
    expect_equal(prior$fn(c(0.25, 0.5, 0.75)), c(-5, 0, 5))
    expect_equal(
      prior$fn(matrix(c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), nrow = 2)),
      matrix(c(-10, -8, -6, -4, -2, 0), nrow = 2)
    )
    expect_snapshot(prior)
  })

  it("catchers issues with prior function output length", {
    fn <- function(x) c(x[1], x[2], x[1] / x[2])
    expect_error(
      create_prior(fn, LETTERS[1:2]),
      "`fn` must return a numeric vector of length 2."
    )
  })

  it("errors if prior returns non-finite values", {
    fn <- function(x) rep(NaN, length(x))
    expect_error(
      create_prior(fn, LETTERS[1:2]),
      "`fn` cannot return non-numeric, missing, or `NaN` values"
    )
    fn <- function(x) c(x[1], NA)
    expect_error(
      create_prior(fn, LETTERS[1:2]),
      "`fn` cannot return non-numeric, missing, or `NaN` values"
    )
  })

  it("errors if prior returns OOB values", {
    fn <- function(x) x
    expect_error(
      create_prior(fn, LETTERS[1:2], lower = 0.5),
      "`fn` must return values within the bounds `lower` and `upper`."
    )
    expect_error(
      create_prior(fn, LETTERS[1:2], upper = 0.5),
      "`fn` must return values within the bounds `lower` and `upper`."
    )
  })
})

# test_that(,{})
