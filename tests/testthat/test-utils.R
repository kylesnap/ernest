test_that("ernest_logging configures logging correctly", {
  withr::local_options(ernest_logging = FALSE)
  config <- ernest_logging(dir = ".", threshold = "INFO", layout = "json")
  expect_s3_class(config, "ernest_logging")
  expect_equal(config$threshold, "INFO")
  expect_equal(config$dir, normalizePath("."))
  expect_equal(config$fileext, ".json")
  expect_equal(getOption("ernest_logging"), config)
  expect_snapshot(config, transform = \(x) {
    sub(config$dir, "PATH", x)
  })

  result <- ernest_logging(dir = FALSE)
  expect_null(result)
  expect_null(getOption("ernest_logging"))
})

test_that("ernest_logging handles invalid directories", {
  withr::local_options(ernest_logging = FALSE)

  expect_snapshot(config <- ernest_logging(dir = "/nonexistent/path"))
  expect_equal(config$dir, tempdir())
})

test_that("logging works during generate() calls", {
  withr::local_options(
    ernest_logging = FALSE,
    rlib_message_verbosity = "default"
  )

  config <- ernest_logging(threshold = "DEBUG", layout = "json")

  # Run sampling with logging enabled
  expect_snapshot(
    {
      sampler <- ernest_sampler(
        gaussian_blobs$log_lik,
        gaussian_blobs$prior,
        n_points = 100
      )
      run <- generate(
        sampler,
        max_iterations = 10,
        seed = 42,
        show_progress = FALSE
      )
    },
    transform = \(x) {
      sub("i Logging run to .+$", "i Logging run to FILE", x)
    }
  )

  log_files <- list.files(
    getOption("ernest_logging")$dir,
    pattern = "ernest_generate_",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(log_files, 1)
  expect_true(file.exists(log_files[1]))
  expect_gt(file.size(log_files[1]), 0)
})

test_that("logging option cleanup works properly", {
  # Store original option
  original_option <- getOption("ernest_logging")

  withr::with_tempdir({
    # Set logging option
    config <- ernest_logging(dir = ".", threshold = "INFO")
    expect_equal(getOption("ernest_logging"), config)

    # Disable logging
    ernest_logging(dir = FALSE)
    expect_null(getOption("ernest_logging"))
  })
})

test_that("check_class works as expected", {
  expect_invisible(check_class(structure(list(), class = "foo"), "foo"))
  expect_invisible(
    check_class(structure(list(), class = c("foo", "bar")), "foo")
  )
  expect_invisible(
    check_class(structure(list(), class = c("foo", "bar")), c("foo", "bar"))
  )
  expect_invisible(check_class(NULL, "foo", allow_null = TRUE))
  expect_snapshot_error(check_class(1, "foo"))
  expect_error(check_class(NULL, "foo", allow_null = FALSE), "not `NULL`")
})

test_that("check_matrix works as expected", {
  mat <- matrix(as.double(1:6), nrow = 2, ncol = 3)
  expect_invisible(check_matrix(mat, nrow = 2, ncol = 3))
  expect_error(check_matrix(mat, nrow = 3, ncol = 2), "dimensions")
  expect_error(check_matrix(matrix("a", 2, 2), nrow = 2, ncol = 2), "matrix")
  mat_nan <- mat
  mat_nan[1, 1] <- NaN
  expect_snapshot_error(check_matrix(mat_nan, nrow = 2, ncol = 3))
  mat_low <- mat
  mat_low[1, 1] <- -10
  expect_error(
    check_matrix(mat_low, nrow = 2, ncol = 3, lower = 0),
    "lower boundary"
  )
  mat_up <- mat
  mat_up[1, 1] <- 100
  expect_error(
    check_matrix(mat_up, nrow = 2, ncol = 3, upper = 10),
    "upper boundary"
  )
})

test_that("check_unique_names works as expected", {
  expect_invisible(check_unique_names(list(a = 1, b = 2)))
  expect_snapshot_error(check_unique_names(list(a = 1, a = 2)))
  expect_error(check_unique_names(list(1, 2)), "unique names")
  expect_error(check_unique_names(list(a = 1, 2)), "unique names")
})
