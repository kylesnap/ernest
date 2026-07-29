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

  # Set up CRAN-compliant daemons
  mirai::daemons(1, dispatcher = FALSE)
  on.exit(mirai::daemons(0), add = TRUE)

  withr::with_options(
    list(ernest.dev_path = tempdir()),
    expect_error(
      expect_warning(
        generate(sampler, parallel = TRUE),
        class = "ernest.on_dev"
      ),
      "Could not find a root 'DESCRIPTION' file"
    )
  )
})

mirai::daemons(1, dispatcher = FALSE)
on.exit(mirai::daemons(0), add = TRUE)

test_that("allocate_nlive is set appropriately", {
  # nlive / nvar > 10L
  allocation <- allocate_nlive(nlive = 307, parallel = 5L, nvar = 5L)
  expect_length(allocation, 5)
  expect_equal(sum(vctrs::list_sizes(allocation)), 307)
  expect_all_equal(vctrs::list_sizes(allocation)[1:2], 62)

  # nlive / nvar < 10L
  expect_warning(
    allocation <- allocate_nlive(nlive = 307, parallel = 7L, nvar = 5L),
    "adjusting `parallel` from 7 to 6"
  )
  expect_length(allocation, 6)
  expect_equal(sum(vctrs::list_sizes(allocation)), 307)
  expect_all_equal(vctrs::list_sizes(allocation)[1], 52)

  # nlive / nvar < 1L
  expect_error(
    allocate_nlive(nlive = 311, parallel = 311L, nvar = 5L),
    "Must have at least one live point within each subrun."
  )
})

describe("generate & mirai", {
  silent_dev_load <- \(...) {
    suppressWarnings(
      expect_run(...),
      class = c("ernest.on_dev", "ernest.min_ess_warning")
    )
  }
  run <- NULL

  it("can run a parallel sampler", {
    run <<- silent_dev_load(
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
    run_cpy <- silent_dev_load(
      log_lik = parallel_lik,
      prior = gaussian_blobs$prior,
      nlive = 300,
      .expected_log_z = gaussian_blobs$log_z_analytic,
      .generate = list(max_iterations = 1000, parallel = TRUE)
    )
    expect_equal(run$rcrd, run_cpy$rcrd)
  })

  it("can run a parallel sampler from an ernest_run", {
    cont_run <- suppressWarnings(
      generate(run, max_iterations = 2000, parallel = 2),
      class = "ernest.on_dev"
    )
    expect_equal(
      field(cont_run$rcrd[1:1000], "log_lik"),
      field(run$rcrd[1:1000], "log_lik")
    )
  })
})
