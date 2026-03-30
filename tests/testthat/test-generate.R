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

  x_rcrd <- as_ernest_rcrd(example_run)
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
    integration <- compute_integral(field(x_rcrd, "log_lik"), example_run$nlive)
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

test_that("generate can continue generating over previous results", {
  continued <- generate(example_run, min_logz = 0.025)
  prev_dead <- example_run$niter - example_run$nlive
  expect_identical(
    example_run$samples$unit_cube[1:prev_dead, ],
    continued$samples$unit_cube[1:prev_dead, ]
  )
})
