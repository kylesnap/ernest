sampler <- NULL
test_that("Set up sampler", {
  expect_no_error(
    sampler <<- ernest_sampler(
      log_lik = gaussian_blobs$log_lik,
      prior = gaussian_blobs$prior
    )
  )
})

describe("create_live", {
  it("generates live points correctly", {
    result <- create_live(sampler$lrps, 10)
    expect_equal(dim(result$unit), c(10, 2))
    expect_equal(
      apply(
        apply(result$unit, 1, gaussian_blobs$prior$fn),
        2,
        gaussian_blobs$log_lik
      ),
      result$log_lik
    )
  })
  it("gives informative error when unit_log_fn fails completely", {
    bad_lik <- new_rwmh_cube(
      unit_log_fn = \(x) stop("Bad Job!"),
      n_dim = 2L
    )
    expect_snapshot(create_live(bad_lik, 10), error = TRUE)
  })
})

#' @srrstats {BS2.1a} check_set_live() ensures that log_lik produces an expected
#' output given some output from the prior transformation, ensuring the two
#' are commensurate. It is called by compile and by ernest_sampler.
describe("check_live_set", {
  reset_live <- function() {
    env_bind(
      sampler$run_env,
      unit = matrix(runif(1000), nrow = 500, ncol = 2),
      log_lik = seq(-10, -1, length.out = 500),
      birth = rep(0L, 500)
    )
  }

  it("passes with valid live set", {
    reset_live()
    expect_silent(check_live_set(sampler))
  })

  it("errors if unit is not a matrix of correct shape", {
    reset_live()
    env_bind(sampler$run_env, unit = runif(20))
    expect_snapshot(check_live_set(sampler), error = TRUE)
  })

  it("errors if unit matrix has wrong dimensions", {
    reset_live()
    env_bind(
      sampler$run_env,
      unit = matrix(runif(1000), nrow = 250, ncol = 4)
    )
    expect_snapshot(check_live_set(sampler), error = TRUE)
  })

  it("errors if unit matrix contains NaN", {
    reset_live()
    trick_mat <- matrix(runif(1000), nrow = 500, ncol = 2)
    trick_mat[5, 2] <- NaN
    env_bind(
      sampler$run_env,
      unit = trick_mat
    )
    expect_snapshot(check_live_set(sampler), error = TRUE)
  })

  it("errors if log_lik is too short", {
    reset_live()
    too_short <- seq(-10, -1, length.out = 499)
    env_bind(
      sampler$run_env,
      unit = matrix(runif(1000), nrow = 500, ncol = 2),
      log_lik = too_short
    )
    expect_snapshot(check_live_set(sampler), error = TRUE)
  })

  it("errors if log_lik contains NaN", {
    reset_live()
    nonfinite <- seq(-10, -1, length.out = 500)
    nonfinite[5] <- NaN
    env_bind(
      sampler$run_env,
      log_lik = nonfinite
    )
    expect_snapshot(check_live_set(sampler), error = TRUE)
  })

  it("errors if log_lik contains Inf", {
    reset_live()
    nonfinite <- seq(-10, -1, length.out = 500)
    nonfinite[5] <- Inf
    env_bind(
      sampler$run_env,
      log_lik = nonfinite
    )
    expect_snapshot(check_live_set(sampler), error = TRUE)
  })

  it("passes if log_lik contains -Inf", {
    reset_live()
    nonfinite <- seq(-10, -1, length.out = 500)
    nonfinite[5] <- -Inf
    env_bind(sampler$run_env, log_lik = nonfinite)
    expect_no_error(check_live_set(sampler))
  })

  it("errors if log_lik is a plateau (all values identical)", {
    reset_live()
    log_lik_plateau <- rep(-10, 500L)
    env_bind(sampler$run_env, log_lik = log_lik_plateau)
    expect_snapshot(check_live_set(sampler), error = TRUE)
  })

  it("warns if log_lik has repeated values but not all identical", {
    reset_live()
    log_lik_repeats <- seq(-10, -1, length.out = 500)
    log_lik_repeats[250:500] <- log_lik_repeats[250]
    env_bind(sampler$run_env, log_lik = log_lik_repeats)
    expect_snapshot_warning(check_live_set(sampler))
  })

  it("errors if birth vector is wrong", {
    sampler <- compile(sampler)
    env_poke(sampler$run_env, "birth", rep(1, 5))
    expect_snapshot(check_live_set(sampler), error = TRUE)

    reset_live()
    env_poke(sampler$run_env, "birth", as.double(rep(0, 10)))
    expect_snapshot(check_live_set(sampler), error = TRUE)
  })
})

describe("compile", {
  it("initializes live points", {
    sampler <- compile(sampler)
    orig_units <- sampler$run_env$unit
    orig_log_lik <- sampler$run_env$log_lik

    expect_equal(dim(orig_units), c(500, 2))
    expected_log_lik <- apply(
      t(apply(orig_units, 1, gaussian_blobs$prior$fn)),
      1,
      gaussian_blobs$log_lik
    )

    expect_equal(orig_log_lik, expected_log_lik)
    expect_equal(sampler$run_env$birth, rep(0L, 500))
    expect_snapshot(sampler)
  })

  it("seed setting with ints produces expected matrices", {
    sampler <- compile(sampler, seed = 42L)
    matrix1 <- env_get(sampler$run_env, "unit")
    env_unbind(sampler$run_env, "unit")

    sampler <- compile(sampler, seed = 42L)
    matrix2 <- env_get(sampler$run_env, "unit")
    env_unbind(sampler$run_env, "unit")
    expect_identical(matrix1, matrix2)

    sampler <- compile(sampler, seed = NULL)
    matrix3 <- env_get(sampler$run_env, "unit")
    env_unbind(sampler$run_env, "unit")
    expect_false(identical(matrix1, matrix3))
  })

  it("ernest_run works with clear = TRUE and FALSE", {
    # Setup a fake ernest_run object
    sampler <- ernest_sampler(
      log_lik = gaussian_blobs$log_lik,
      prior = gaussian_blobs$prior,
      n_points = 10
    )
    sampler <- compile(sampler, seed = 123L)
    run <- generate(sampler, max_iterations = 1000L)

    # clear = TRUE should call compile.ernest_sampler
    expect_s3_class(compile(run, clear = TRUE), "ernest_sampler")

    # clear = FALSE should restore live points
    expect_s3_class(compile(run, clear = FALSE), "ernest_run")
  })
})
