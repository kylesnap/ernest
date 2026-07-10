test_that("Crated func. envs. are attached to the search path", {
  # Median and qnorm are both in the stats package
  parallel_lik <- parallel_likelihood(
    function(x) {
      median(x)
    }
  )
  expect_equal(parallel_lik(c(1, 2, 3)), 2)

  parallel_v_prior <- parallel_prior(
    vectorized_fn = function(x) qnorm(x),
    .names = c("A", "B", "C")
  )
  expect_equal(
    parallel_v_prior$fn(matrix(c(0.1, 0.5, 0.9), nrow = 1)),
    matrix(stats::qnorm(c(0.1, 0.5, 0.9)), nrow = 1)
  )
})

# Portable version of the two Gaussian blobs test likelihood
parallel_lik <- parallel_likelihood(
  vectorized_fn = function(x) {
    dx1 <- -0.5 * mahalanobis(x, mu1, sigma_inv, inverted = TRUE)
    dx2 <- -0.5 * mahalanobis(x, mu2, sigma_inv, inverted = TRUE)
    matrixStats::colLogSumExps(rbind(dx1, dx2))
  },
  sigma = 0.1,
  mu1 = c(1, 1),
  mu2 = -c(1, 1),
  sigma_inv = diag(2) / 0.1**2
)

describe("runs_in_parallel", {
  it("returns FALSE for non-parallelizable samplers", {
    # Likelihood not crated
    expect_false(runs_in_parallel(ernest_sampler(
      gaussian_blobs$log_lik,
      gaussian_blobs$prior
    )))
    # Prior not crated
    expect_false(runs_in_parallel(ernest_sampler(
      parallel_lik,
      prior = create_prior(\(x) x * 10 - 5, names = c("A", "B"))
    )))
  })

  describe("returns whether daemons are set", {
    sampler <- ernest_sampler(
      parallel_lik,
      gaussian_blobs$prior
    )
    expect_false(runs_in_parallel(sampler))

    with(
      mirai::daemons(1, dispatcher = FALSE),
      expect_true(runs_in_parallel(sampler))
    )
  })
})

#' Set up CRAN-compliant daemons
mirai::daemons(1, dispatcher = FALSE)
on.exit(mirai::daemons(0), add = TRUE)

test_that("ernest_sampler prints messages while in parallel", {
  sampler <- ernest_sampler(
    log_lik = parallel_lik,
    prior = gaussian_blobs$prior,
    nlive = 300
  )

  expect_error(
    generate(sampler, max_iterations = 10, batch_size = 301),
    "`batch_size` must be a whole number between 1 and 300"
  )

  withr::with_options(
    list(rlib_message_verbosity = "quiet"),
    expect_message(
      generate(sampler, max_iterations = 10, batch_size = 1),
      "set `batch_size` to larger than 1"
    )
  )
})

test_that("ernest_sampler can be run in parallel", {
  Sys.setenv(DEBUGME = "ernest")
  run1 <- expect_run(
    log_lik = parallel_lik,
    prior = gaussian_blobs$prior,
    sampler = multi_ellipsoid(),
    nlive = 300,
    .expected_log_z = gaussian_blobs$log_z_analytic,
    .generate = list(min_logz = 0.5, batch_size = 2)
  )

  run2 <- generate(run1, max_iterations = run1$niter + 1000, batch_size = 2)
  expect_identical(run1$rcrd[1:run1$niter], run2$rcrd[1:run1$niter])
})
