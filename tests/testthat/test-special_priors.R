set.seed(42)

test_matrix <- matrix(c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), nrow = 2)

describe("create_normal_prior", {
  it("reports poor parameters", {
    expect_snapshot(create_normal_prior(sd = -1), error = TRUE)
  })

  it("validly returns untruncated priors", {
    prior <- create_normal_prior(mean = c(0, 0.5, 1))
    expect_s3_class(prior, c("normal_prior", "ernest_prior"), exact = TRUE)
    expect_equal(
      prior$names,
      c("Normal_0.0_1.0", "Normal_0.5_1.0", "Normal_1.0_1.0")
    )
    expect_equal(attr(prior, "n_dim"), 3L)
    expect_equal(prior$lower, c(-Inf, -Inf, -Inf))
    expect_equal(prior$upper, c(Inf, Inf, Inf))

    expect_equal(prior$fn(c(0.5, 0.5, 0.5)), c(0.0, 0.5, 1.0))
    expect_equal(
      prior$fn(test_matrix),
      t(apply(test_matrix, 1, qnorm, mean = c(0.0, 0.5, 1.0)))
    )
  })
  it("validly returns truncated priors", {
    prior <- create_normal_prior(
      mean = c(0, 0.5, 1),
      lower = 0,
      upper = c(Inf, 5, Inf)
    )
    expect_s3_class(
      prior,
      c("trunc_prior", "normal_prior", "ernest_prior"),
      exact = TRUE
    )
    expect_equal(
      prior$names,
      c("Normal_0.0_1.0", "Normal_0.5_1.0", "Normal_1.0_1.0")
    )
    expect_equal(attr(prior, "n_dim"), 3L)
    expect_equal(prior$lower, c(0, 0, 0))
    expect_equal(prior$upper, c(Inf, 5, Inf))

    expect_equal(
      prior$fn(c(0.5, 0.5, 0.5)),
      extraDistr::qtnorm(
        0.5,
        mean = c(0.0, 0.5, 1.0),
        a = 0,
        b = c(Inf, 5, Inf)
      )
    )
    expect_equal(
      prior$fn(test_matrix),
      t(apply(
        test_matrix,
        1,
        extraDistr::qtnorm,
        mean = c(0.0, 0.5, 1.0),
        a = 0,
        b = c(Inf, 5, Inf)
      ))
    )
  })

  it("recycles vector parameters to the expected length", {
    prior <- create_normal_prior(mean = c(0, 0, 0))
    prior2 <- create_normal_prior(
      names = c(
        "Normal_0.0_1.0...1",
        "Normal_0.0_1.0...2",
        "Normal_0.0_1.0...3"
      )
    )
    prior3 <- create_normal_prior(sd = c(1, 1, 1))
    expect_identical(prior2, prior)
    expect_identical(prior3, prior)
  })
})

describe("create_uniform_prior", {
  it("validly returns priors", {
    prior <- create_uniform_prior(lower = c(0, 0, -10), upper = 1)
    expect_s3_class(prior, c("uniform_prior", "ernest_prior"), exact = TRUE)
    expect_equal(
      prior$names,
      c("Uniform_0.0_1.0...1", "Uniform_0.0_1.0...2", "Uniform_m10.0_1.0")
    )
    expect_equal(attr(prior, "n_dim"), 3L)
    expect_equal(prior$lower, c(0, 0, -10))
    expect_equal(prior$upper, c(1, 1, 1))

    expect_equal(prior$fn(c(0.5, 0.5, 0.5)), c(0.5, 0.5, -4.5))
    expect_equal(
      prior$fn(test_matrix),
      t(apply(test_matrix, 1, qunif, min = c(0, 0, -10), max = 1))
    )
  })

  it("recycles vector parameters to the expected length", {
    prior <- create_uniform_prior(lower = c(0, 0, 0))
    prior2 <- create_uniform_prior(
      names = c(
        "Uniform_0.0_1.0...1",
        "Uniform_0.0_1.0...2",
        "Uniform_0.0_1.0...3"
      )
    )
    prior3 <- create_uniform_prior(upper = c(1, 1, 1))
    expect_identical(prior2, prior)
    expect_identical(prior3, prior)
  })
})
