withr::local_seed(42)

test_that("get_points returns expected values", {
  expect_equal(
    get_points(c(10, 9, 8, 7, 6, 5, 4), 3, TRUE),
    c(3, 3, 3, 3, 3, 2, 1)
  )
  expect_equal(
    get_points(c(10, 10, 9, 8, 7, 7, 6), 3, TRUE),
    c(3, 2, 3, 3, 3, 2, 1)
  )
  expect_equal(
    get_points(c(10, 10, 9, 8, 7, 7, 6), 3, FALSE),
    c(3, 2, 3, 3, 3, 2, 3)
  )
  expect_equal(
    get_points(c(10, 10, 10, 10, 10, 10, 10), 3, TRUE),
    c(3, 2, 1, 1, 3, 2, 1)
  )
})

test_that("compute_integral correctly calculates values", {
  #' Tested against sample run from PolyChord.
  gold <- readRDS(test_path("calculate-gold.rds")) |>
    as.list()
  rcrd <- new_ernest_rcrd(
    unit = matrix(0, nrow = length(gold$log_lik), ncol = 2),
    log_lik = gold$log_lik,
    id = c(
      rep(seq(250), length.out = length(gold$log_lik) - 250),
      rev(seq(250))
    ),
    nlive = gold$points,
    evals = c(
      rep(1, length.out = length(gold$log_lik) - 250),
      rep(0, 250)
    ),
    birth_lik = rep(-Inf, length.out = length(gold$log_lik))
  )

  obj <- compute_integral(rcrd)
  expect_mapequal(
    obj[c("log_lik", "log_volume", "log_weight", "log_evidence")],
    gold[c("log_lik", "log_volume", "log_weight", "log_evidence")]
  )
  expect_warning(
    get_log_vol(rev(rcrd)),
    "Log-weight estimates are unreliable."
  )
})

describe("calculate", {
  data(example_run)
  nsamp <- example_run$nlive + example_run$niter
  expect_shape_rvar <- function(object, ndraws, dim, ...) {
    object <- posterior::draws_of(object)
    expect_shape(object, dim = c(ndraws, dim))
  }

  it("works when ndraws = 0", {
    calc <- calculate(example_run, ndraws = 0)
    expect_s3_class(calc, c("ernest_estimate", "tbl_df"))
    expect_identical(attr(calc, "ndraws"), 0L)
    expect_s3_class(attr(calc, "log_z_dist"), c("distribution"))

    expect_equal(calc$log_lik, field(example_run$rcrd, "log_lik"))
    expect_shape_rvar(calc$log_volume, 1, nsamp)
    expect_shape_rvar(calc$log_weight, 1, nsamp)
    expect_shape_rvar(calc$log_evidence, 1000, nsamp)

    expected <- compute_integral(example_run$rcrd)
    expect_equal(
      unname(drop(posterior::draws_of(calc$log_weight))),
      expected$log_weight
    )
    expect_equal(
      mean(calc$log_evidence),
      expected$log_evidence,
      tolerance = 1e-3
    )
    expected_sd <- sqrt(expected$log_evidence_var)
    observed_sd <- posterior::sd(calc$log_evidence)
    expect_all_true(
      # Tolerance around uncertainty.
      abs(observed_sd - expected_sd) < 2 * expected_sd
    )
    expect_snapshot(calc)
  })

  it("works when ndraws = 1 (default)", {
    calc <- calculate(example_run, ndraws = 1)
    expect_identical(attr(calc, "ndraws"), 1L)
    expect_null(attr(calc, "log_z_dist"))

    expect_shape_rvar(calc$log_volume, 1, nsamp)
    expect_shape_rvar(calc$log_weight, 1, nsamp)
    expect_shape_rvar(calc$log_evidence, 1, nsamp)
    expect_snapshot(calc)
  })

  it("works when ndraws = 1000 (default)", {
    calc <- calculate(example_run)
    expect_identical(attr(calc, "ndraws"), 1000L)
    expect_null(attr(calc, "log_z_dist"))

    expect_shape_rvar(calc$log_volume, 1000, nsamp)
    expect_shape_rvar(calc$log_weight, 1000, nsamp)
    expect_shape_rvar(calc$log_evidence, 1000, nsamp)

    expected <- compute_integral(example_run$rcrd)
    expect_shape(posterior::draws_of(calc$log_evidence), dim = c(1000, nsamp))
    expect_equal(
      mean(calc$log_evidence),
      expected$log_evidence,
      tolerance = 1e-3
    )
    expect_snapshot(calc)
  })
})
