# ernest_logging handles invalid directories

    Code
      config <- ernest_logging(dir = "/nonexistent/path")
    Condition
      Warning:
      Can't find the filepath `dir`. Using `tempdir()` instead.

# logging works during generate() calls

    Code
      sampler <- ernest_sampler(gaussian_blobs$log_lik, gaussian_blobs$prior,
      n_points = 100)
      run <- generate(sampler, max_iterations = 10, show_progress = FALSE)
    Message
      i Created 100 live points.
      i Logging run to FILE
      v `max_iterations` reached (10).

# check_class works as expected

    `1` must be an object with class foo, not the number 1.

# check_matrix works as expected

    `mat_nan` must not contain missing or `NaN` values.

# check_unique_names works as expected

    All elements of `list(a = 1, a = 2)` must have unique names.
    x Repeated names: a

