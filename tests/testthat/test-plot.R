test_that("ernest_run can be plotted", {
  run <- readRDS(test_path("./example_run.rds"))
  tbl <- calc_var_tbl(
    run$log_volume,
    run$log_evidence,
    run$log_evidence_var,
    run$log_weight,
    run$log_lik
  )
  expect_equal(tbl$fill_name, expression(hat(sigma[Z])))
  expect_equal(tbl$fill_limits, c(3, 2, 1))
  expect_equal(
    tbl$fill_labels,
    c(
      expression(3 * sigma),
      expression(2 * sigma),
      expression(1 * sigma)
    )
  )
  expect_named(
    tbl$df,
    c("log_vol", ".label", ".var", ".lower", ".upper", ".width")
  )

  vdiffr::expect_doppelganger("ernest_run plot", plot(run))
})

test_that("ernest_run can be calculated, then plotted", {
  run <- readRDS(test_path("./example_run.rds"))
  set.seed(42)
  calc_1 <- calculate(run, ndraws = 1)
  expect_warning(
    tbl_1 <- calc_hdi_tbl(calc_1)
  )
  expect_named(
    tbl_1$df,
    c("log_vol", ".label", ".var", ".lower", ".upper", ".width")
  )
  expect_equal(tbl_1$fill_name, expression(hat(sigma[Z])))

  calc_2 <- calculate(run, ndraws = 2)
  expect_warning(
    tbl_2 <- calc_hdi_tbl(calc_2)
  )
  expect_named(
    tbl_2$df,
    c("log_vol", ".label", ".var", ".lower", ".upper", ".width")
  )
  expect_equal(tbl_2$fill_name, expression(hat(sigma[Z])))

  calc_100 <- calculate(run, ndraws = 100)
  tbl_100 <- calc_hdi_tbl(calc_100)
  expect_named(
    tbl_100$df,
    c("log_vol", ".label", ".var", ".lower", ".upper", ".width"),
    ignore.order = TRUE
  )
  expect_equal(tbl_100$fill_name, "HDCI")

  skip_on_ci()
  skip_on_cran()
  skip_on_covr()
  expect_warning(vdiffr::expect_doppelganger(
    "ernest_estimate(ndraws = 1)",
    plot(calculate(run, ndraws = 1))
  ))

  expect_warning(vdiffr::expect_doppelganger(
    "ernest_estimate(ndraws = 2)",
    plot(calculate(run, ndraws = 2))
  ))

  vdiffr::expect_doppelganger(
    "ernest_estimate(ndraws = 100)",
    plot(calculate(run, ndraws = 100))
  )
})

test_that("ernest_run can be plotted with ndraws", {
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()
  run <- readRDS(test_path("./example_run.rds"))
  set.seed(42)
  vdiffr::expect_doppelganger(
    "ernest_run(ndraws = 100)",
    plot(run, ndraws = 100)
  )
})
