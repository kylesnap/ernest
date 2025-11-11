set.seed(42)

test_that("ernest_sampler returns an ernest_sampler object", {
  prior <- create_uniform_prior(.n_dim = 2, lower = -1, upper = 1)
  ll_fn <- function(x) -sum(x^2)
  sampler <- ernest_sampler(ll_fn, prior, n_points = 50, seed = 42)
  expect_s3_class(sampler, "ernest_sampler")
})

test_that("ernest_sampler errors with invalid prior", {
  ll_fn <- function(x) -sum(x^2)
  expect_snapshot_error(
    ernest_sampler(ll_fn, prior = \(x) runif(x), n_points = 10, seed = 42)
  )
})

#' @srrstats {BS2.13} Test that R can produce fully-verbose output when
#' requested (messages are turned off for all other tests).
cli::test_that_cli("Progress bar can be printed", {
  skip_snapshot()
  sampler <- ernest_sampler(
    gaussian_blobs$log_lik,
    gaussian_blobs$prior,
    n_points = 500,
    seed = 42
  )
  withr::local_options(cli.progress_handlers_only = "logger")

  expect_snapshot(
    generate(sampler, max_iterations = 1),
    transform = \(lines) {
      gsub("^.+cli.+$", "cli-line", lines)
    }
  )
})

cli::test_that_cli("Fully-verbose output", {
  skip_snapshot()
  sampler <- ernest_sampler(
    gaussian_blobs$log_lik,
    gaussian_blobs$prior,
    n_points = 500,
    seed = 42
  )
  withr::local_options(rlib_message_verbosity = "verbose")

  expect_snapshot(
    generate(sampler, max_iterations = 1000)
  )
})
