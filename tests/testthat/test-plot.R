data(example_run)

test_that("plotting an ernest_run object", {
  data(example_run)
  tbl <- calc_var_tbl(
    example_run$log_volume,
    example_run$log_evidence,
    example_run$log_evidence_var,
    example_run$log_weight,
    example_run$log_lik
  )
  expect_equal(tbl$fill_name, expression(hat(sigma[Z])))
  expect_equal(tbl$fill_limits, c(3, 2, 1))
  expect_equal(
    tbl$fill_labels,
    c(expression(3 * sigma), expression(2 * sigma), expression(1 * sigma))
  )
  expect_named(
    tbl$df,
    c("log_vol", ".label", ".var", ".lower", ".upper", ".width")
  )

  skip_extended_test()
  vdiffr::expect_doppelganger("ernest_run plot", plot(example_run))
})

test_that("plotting an ernest_estimate object", {
  set.seed(42)
  calc_1 <- calculate(example_run, ndraws = 1)
  expect_snapshot_warning(tbl_1 <- calc_hdi_tbl(calc_1))
  expect_named(
    tbl_1$df,
    c('.lower', '.var', '.upper', '.width', 'log_vol', '.label')
  )
  expect_equal(tbl_1$fill_name, "MCI")

  calc_2 <- calculate(example_run, ndraws = 2)
  expect_snapshot_warning(tbl_2 <- calc_hdi_tbl(calc_2))
  expect_named(
    tbl_2$df,
    c('.lower', '.var', '.upper', '.width', 'log_vol', '.label')
  )
  expect_equal(tbl_2$fill_name, "MCI")

  calc_100 <- calculate(example_run, ndraws = 100)
  tbl_100 <- calc_hdi_tbl(calc_100)
  expect_named(
    tbl_100$df,
    c('.lower', '.var', '.upper', '.width', 'log_vol', '.label')
  )
  expect_equal(tbl_100$fill_name, "MCI")

  skip_extended_test()
  expect_warning(vdiffr::expect_doppelganger(
    "ernest_estimate(ndraws = 1)",
    plot(calculate(example_run, ndraws = 1))
  ))

  expect_warning(vdiffr::expect_doppelganger(
    "ernest_estimate(ndraws = 2)",
    plot(calculate(example_run, ndraws = 2))
  ))

  vdiffr::expect_doppelganger(
    "ernest_estimate(ndraws = 500)",
    plot(calculate(example_run, ndraws = 500))
  )
})

test_that("ernest_run can be plotted after simulation", {
  data(example_run)
  set.seed(42)
  skip_extended_test()
  vdiffr::expect_doppelganger(
    "ernest_run(ndraws = 500)",
    plot(example_run, ndraws = 500)
  )
})
