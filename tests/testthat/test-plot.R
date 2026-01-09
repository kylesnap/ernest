describe("plotting an ernest_estimate object", {
  set.seed(42)
  calc_1 <- calculate(example_run, ndraws = 1)
  calc_100 <- calculate(example_run, ndraws = 100)

  it("plots evidence", {
    vdiffr::expect_doppelganger(
      "evidence ndraws = 100",
      autoplot(calc_100, which = "evidence")
    )
  })

  it("plots weight", {
    vdiffr::expect_doppelganger(
      "weight ndraws = 100",
      autoplot(calc_100, which = "weight")
    )
  })

  it("plots likelihood", {
    vdiffr::expect_doppelganger(
      "likelihood ndraws = 100",
      autoplot(calc_100, which = "likelihood")
    )
  })

  it("plots everything", {
    vdiffr::expect_doppelganger(
      "all ndraws = 0",
      plot(calculate(example_run, ndraws = 0))
    )

    vdiffr::expect_doppelganger(
      "all ndraws = 1",
      plot(calc_1)
    )

    vdiffr::expect_doppelganger(
      "all ndraws = 100",
      plot(calc_100)
    )

    vdiffr::expect_doppelganger(
      "no evidence ndraws = 1",
      plot(calc_1, which = c("likelihood", "weight"))
    )
  })
})

describe("plotting an ernest_run object", {
  data(example_run)
  it("plots evidence", {
    vdiffr::expect_doppelganger(
      "evidence ernest_run",
      autoplot(example_run, which = "evidence")
    )
  })

  it("plots weights", {
    vdiffr::expect_doppelganger(
      "weight ernest_run",
      plot(example_run, which = "weight")
    )
  })

  it("plots likelihood", {
    vdiffr::expect_doppelganger(
      "likelihood ernest_run",
      plot(example_run, which = "likelihood")
    )
  })

  it("plots everything", {
    vdiffr::expect_doppelganger(
      "all ernest_run",
      plot(example_run, which = c("evidence", "weight", "likelihood"))
    )

    vdiffr::expect_doppelganger(
      "no evidence ernest_run",
      plot(example_run, which = c("weight", "likelihood"))
    )
  })
})

test_that("ernest_run can be plotted after simulation", {
  data(example_run)
  set.seed(42)

  vdiffr::expect_doppelganger(
    "ernest_run(ndraws = 100)",
    plot(example_run, ndraws = 100)
  )
})
