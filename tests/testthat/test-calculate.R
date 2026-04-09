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
  expect_equal_rvar <- function(object, expected, ...) {
    object <- unname(drop(posterior::draws_of(object)))
    expect_equal(object, expected, ...)
  }

  it("works when ndraws = 0", {
    calc <- calculate(example_run, ndraws = 0)
    expect_s3_class(calc, "ernest_estimate")
    expect_identical(attr(calc, "ndraws"), 0L)
    expect_equal(calc$log_lik, vctrs::field(example_run$rcrd, "log_lik"))
    expect_shape(calc, nrow = nsamp)

    expected <- compute_integral(example_run$rcrd)
    expect_equal_rvar(calc$log_weight, expected$log_weight)
    expect_shape(posterior::draws_of(calc$log_evidence), dim = c(1000, nsamp))
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

  it("works when ndraws = 1000 (default)", {
    calc <- calculate(example_run)
    calc1 <- calculate(example_run, ndraws = 0)
    expect_identical(attr(calc, "ndraws"), 1000L)
    expect_equal(calc$log_lik, vctrs::field(example_run$rcrd, "log_lik"))
    expect_shape(calc, nrow = nsamp)

    expected <- compute_integral(example_run$rcrd)
    expect_shape(posterior::draws_of(calc1$log_evidence), dim = c(1000, nsamp))
    expect_equal(
      mean(calc$log_evidence),
      expected$log_evidence,
      tolerance = 1e-3
    )
    expect_snapshot(calc)
  })
})
