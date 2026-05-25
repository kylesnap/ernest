parallel_lik <- NULL
parallel_pt <- NULL

test_that("parallel ernest_likelihood works", {
  parallel_lik <<- parallel_likelihood(
    vectorized_fn = function(x) {
      sigma <- 0.1
      mu1 <- c(1, 1)
      mu2 <- -c(1, 1)
      sigma_inv <- diag(2) / 0.1**2

      dx1 <- -0.5 * stats::mahalanobis(x, c(1, 1), sigma_inv, inverted = TRUE)
      dx2 <- -0.5 * stats::mahalanobis(x, c(-1, -1), sigma_inv, inverted = TRUE)
      matrixStats::colLogSumExps(rbind(dx1, dx2))
    }
  )
  expect_s3_class(parallel_lik, c("ernest_likelihood", "crate"))
})

test_that("parallel ernest_prior works", {
  parallel_pt <<- parallel_prior(
    function(x) {
      (x * 10) - 5
    },
    .names = c("a", "b")
  )
  expect_s3_class(parallel_pt, c("crated_prior", "ernest_prior"))
  expect_s3_class(attr(parallel_pt, "body"), "crate")
})

describe("pgenerate", {
  it("fails when log_lik is not a crate", {
    sampler <- ernest_sampler(\(x) sum(x), parallel_pt)
    expect_error(
      generate(sampler, parallel = TRUE),
      "portable `log_lik` function"
    )
  })

  it("fails when prior is not a crate", {
    sampler <- ernest_sampler(
      parallel_lik,
      create_prior(\(x) cumsum(x), names = LETTERS[1:2])
    )
    expect_error(
      generate(sampler, parallel = TRUE),
      "portable `prior` function"
    )
  })

  it("fails when daemons are not set", {
    sampler <- ernest_sampler(parallel_lik, parallel_pt)
    expect_error(
      generate(sampler, parallel = TRUE),
      "No daemons set."
    )
  })
})

describe("generate & mirai", {
  # Set up CRAN-compliant daemons
  mirai::daemons(1, dispatcher = FALSE)
  on.exit(mirai::daemons(0), add = TRUE)

  run <- NULL
  expected_log_z <- log(2.0 * 2.0 * pi * 0.1 * 0.1 / 100)

  it("can run a parallel sampler", {
    sampler <- ernest_sampler(parallel_lik, parallel_pt, nlive = 500, seed = 42)
    run <<- generate(
      sampler,
      parallel = c(250, 250),
      max_iterations = 1000,
      min_logz = 0.1
    )
    print(run$parallel)
    # log_z <- run$log_evidence
    # log_z_err <- run$log_evidence_err
    # delta_log_z <- abs(log_z - expected_log_z)
    # expect_lte(delta_log_z, 3 * log_z_err)
  })

  it("can run a parallel sampler with existing rcrd", {})
})
