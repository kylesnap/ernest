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

test_that("parallelization checks for portable functions and daemons", {
  expect_error(
    generate(
      ernest_sampler(gaussian_blobs$log_lik, gaussian_blobs$prior),
      parallel = TRUE
    ),
    "portable `log_lik` function"
  )

  expect_error(
    generate(
      ernest_sampler(
        parallel_lik,
        prior = create_prior(\(x) x * 10 - 5, names = c("A", "B"))
      ),
      parallel = TRUE
    ),
    "portable `prior` function."
  )

  sampler <- ernest_sampler(parallel_lik, gaussian_blobs$prior)
  expect_error(
    generate(sampler, parallel = TRUE),
    "No daemons set."
  )
})

test_that("allocate_nlive is set appropriately", {
  # parallel < nlive
  ids <- as.character(seq_len(301))
  allocation <- allocate_nlive(ids, parallel = 2, nvar = 3L)
  lengths <- vctrs::list_sizes(allocation)
  expect_equal(sum(lengths), 301)
  expect_equal(lengths[1], 151)

  # parallel == nlive
  expect_warning(
    allocation <- allocate_nlive(ids, parallel = 301, nvar = 3L),
    "Should you decrease the number of `.parallel` workers?"
  )
  lengths <- vctrs::list_sizes(allocation)
  expect_equal(sum(lengths), 301)
  expect_equal(lengths[1], 5)
  expect_all_equal(lengths[-1], 4)
})

describe("generate & mirai", {
  # Set up CRAN-compliant daemons
  mirai::daemons(1, dispatcher = FALSE)
  on.exit(mirai::daemons(0), add = TRUE)
  run <- NULL

  it("can run a parallel sampler", {
    run <<- expect_run(
      log_lik = parallel_lik,
      prior = gaussian_blobs$prior,
      nlive = 300,
      .expected_log_z = gaussian_blobs$log_z_analytic,
      .generate = list(max_iterations = 1000, parallel = TRUE)
    )
    glanced <- glance(run)
    glanced$seed <- NULL
    expect_mapequal(run$.parallel, glanced)
  })

  it("respects a set seed", {
    run_cpy <- expect_run(
      log_lik = parallel_lik,
      prior = gaussian_blobs$prior,
      nlive = 300,
      .expected_log_z = gaussian_blobs$log_z_analytic,
      .generate = list(max_iterations = 1000, parallel = TRUE)
    )
    expect_equal(run$rcrd, run_cpy$rcrd)
  })

  it("can run a parallel sampler from an ernest_run", {
    cont_run <- generate(run, max_iterations = 2000, parallel = 2)
    expect_equal(
      field(cont_run$rcrd[1:1000], "log_lik"),
      field(run$rcrd[1:1000], "log_lik")
    )
  })
})
