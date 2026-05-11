data(example_run)

describe("new_generate_control", {
  it("catches invalid or empty criteria", {
    expect_error(
      new_generate_control(NULL, NULL, 0),
      "Can't perform nested sampling without any stopping criteria."
    )
    expect_error(
      new_generate_control(-1, NULL, 0),
      "a whole number larger than or equal to 1 or `NULL`"
    )
  })

  it("returns expected defaults", {
    expect_mapequal(
      new_generate_control(NULL, NULL, 0.05),
      list(
        max_iterations = .Machine$integer.max,
        max_evaluations = .Machine$integer.max,
        min_logz = 0.05,
        last_criterion = -1e300,
        log_vol = 0,
        log_z = -1e300,
        cur_iter = 0L,
        cur_eval = 0L
      )
    )
  })

  x_rcrd <- example_run$rcrd
  it("fails to set an invalid continuation state", {
    niter <- example_run$niter
    neval <- example_run$neval

    expect_error(
      new_generate_control(niter, neval + 1L, 0, prev_run = x_rcrd),
      "`max_iterations` must be strictly larger"
    )

    expect_error(
      new_generate_control(niter + 1L, neval, 0, x_rcrd),
      "`max_evaluations` must be strictly larger"
    )

    expect_error(
      new_generate_control(niter + 1L, neval + 1L, 0.05, prev_run = x_rcrd),
      "`min_logz` must be strictly smaller"
    )
  })

  it("sets the continuation state for an existing run", {
    integration <- compute_integral(example_run$rcrd)
    niter <- example_run$niter

    expect_mapequal(
      new_generate_control(NULL, NULL, 0.01, prev_run = x_rcrd),
      list(
        max_iterations = .Machine$integer.max,
        max_evaluations = .Machine$integer.max,
        min_logz = 0.01,
        last_criterion = integration$log_lik[[niter]],
        log_vol = integration$log_vol[[niter]],
        log_z = integration$log_evidence[[niter]],
        cur_iter = as.integer(niter),
        cur_eval = as.integer(example_run$neval)
      )
    )
  })
})

test_that("generate can continue generating from previous results", {
  continued <- generate(example_run, min_logz = 0.025)
  prev_dead <- example_run$niter - example_run$nlive
  expect_identical(example_run$rcrd[1:prev_dead], continued$rcrd[1:prev_dead])

  continued <- generate(
    example_run,
    max_evaluations = round(example_run$neval * 1.1),
    min_logz = 0
  )
  expect_identical(example_run$rcrd[1:prev_dead], continued$rcrd[1:prev_dead])
})

test_that("generate can start a run with a fresh sampler", {
  continued <- generate(example_run, max_iterations = 1000, clear = TRUE)
  expect_equal(example_run$rcrd[1:1000], continued$rcrd[1:1000])
})

#' @srrstats {G5.6b, G5.9, G5.9a, G5.9b} Tests that parameters are
#' recovered under different seeds and with random noise added to the log-lik.
test_that("different seeds and noise levels don't impact evidence estimates", {
  expect_gaussian_run(rwmh_cube(), .seed = 24L)
  sqrt_eps <- sqrt(.Machine$double.eps)
  noisy_gaussian_blob_ll <- function(x) {
    ll <- gaussian_blobs$log_lik(x)
    ll + rnorm(length(ll), mean = 0, sd = sqrt_eps)
  }
  expect_run(
    log_lik = create_likelihood(vectorized_fn = noisy_gaussian_blob_ll),
    prior = gaussian_blobs$prior,
    sampler = rwmh_cube(),
    nlive = 100,
    .expected_log_z = gaussian_blobs$log_z_analytic,
    .seed = NA
  )
})

test_that("Reproducing a ernest_sampler saved to disk", {
  withr::local_file("sampler.rds")
  sampler <- ernest_sampler(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior,
    seed = 42
  )
  run1 <- generate(sampler, max_iterations = 100)

  saveRDS(sampler, "sampler.rds")
  f_sampler <- readRDS("sampler.rds")
  f_run1 <- generate(f_sampler, max_iterations = 100)
  expect_identical(run1$log_volume, f_run1$log_volume)
  expect_identical(
    field(run1$rcrd, "log_lik"),
    field(f_run1$rcrd, "log_lik")
  )
  expect_identical(run1$rcrd, f_run1$rcrd)
})

test_that("`seed` is preserved across runs when set to NA", {
  old_seed <- .Random.seed
  run1 <- expect_gaussian_run(
    sampler = rwmh_cube(),
    .seed = NA,
    .generate = list(max_iterations = 200L)
  )
  new_seed <- .Random.seed
  expect_identical(old_seed, new_seed)
})

#' Special tests: Perfectly flat and nearly flat likelihoods
#'
#' @srrstats {G5.8, G5.8d} Covers situations in which the likelihood starts as
#' flat (caught by compile) or becomes flat after many iterations.
NULL

test_that("Ernest halts and warns when ll becomes flat during a run", {
  ll_flat <- \(theta) {
    if (any(theta > 0)) {
      return(0)
    }
    gaussian_blobs$log_lik(theta)
  }

  expect_warning(
    sampler <- ernest_sampler(ll_flat, prior = gaussian_blobs$prior, seed = 42),
    "`log_lik` may contain a likelihood plateau"
  )
  expect_warning(
    expect_warning(
      run <- generate(sampler),
      "`log_lik` may contain a likelihood plateau"
    ),
    "Stopping run due to a likelihood plateau at 0"
  )
  expect_all_equal(tail(field(run$rcrd, "log_lik"), 100), 0)
})

#' @srrstats {G5.0, G5.6, G5.6a, BS7.2} Use the NIST dataset `mcmc01` to test
#' ernest's ability to recover the parameters of a known distribution. Uses the
#' certified 95% posterior quantile.
#' @srrstats {BS7.4, BS7.4a} Also test to ensure that ernest's output behaves on
#' the same scale as input data wrapped in the likelihood function.
#' From: https://www.itl.nist.gov/div898/strd/mcmc/mcmc01_cv.html
NULL

describe("Parameter recovery", {
  # NIST mcmc01: likelihood and prior
  y <- c(
    100000000.2,
    100000000.1,
    100000000.3,
    100000000.1,
    100000000.3,
    100000000.1,
    100000000.3,
    100000000.1,
    100000000.3,
    100000000.1,
    100000000.3
  )

  log_lik <- function(theta) {
    if (theta[2] <= 0) {
      return(-Inf)
    }
    sum(dnorm(y, mean = theta[1], sd = theta[2], log = TRUE))
  }

  prior <- create_uniform_prior(
    lower = c(99999999, 0.01),
    upper = c(100000001, 1),
    names = c("mu", "sigma")
  )

  # Certified posterior median and 95% interval for mean (from NIST)
  mean_median <- 100000000.200000000000000
  mean_lower <- 100000000.132819085883166
  mean_upper <- 100000000.267180914116834

  # Certified posterior median and 95% interval for sd (from NIST)
  sd_median <- 0.108372230793914
  sd_lower <- 0.069871704416342
  sd_upper <- 0.175493354741336

  sampler <- ernest_sampler(
    log_lik,
    prior,
    seed = 42
  )
  run <- generate(sampler)

  it("Recovers parameters in the ernest_run object", {
    draws <- as_draws(run) |>
      posterior::resample_draws()

    median_mean <- median(posterior::subset_draws(draws, "mu"))
    expect_gt(median_mean, mean_lower)
    expect_lt(median_mean, mean_upper)

    median_sd <- median(posterior::subset_draws(draws, "sigma"))
    expect_gt(median_sd, sd_lower)
    expect_lt(median_sd, sd_upper)
  })
})
