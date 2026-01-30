data(example_run)

describe("ernest_run", {
  it("has the correct meta-class", {
    expect_s3_class(example_run, c("ernest_run", "ernest_sampler"))
    expect_true(is.list(example_run))
    expect_equal(attr(example_run, "seed"), 42)
    expect_type(example_run$niter, "integer")
    expect_gt(example_run$neval, 0L)
    expect_lt(example_run$log_evidence, 0)
    expect_gt(example_run$log_evidence_err, 0)
    expect_type(example_run$information, "double")
  })
  total_length <- example_run$niter + example_run$nlive

  it("Stores samples", {
    expect_named(example_run$samples, c("original", "unit_cube"))
    expect_identical(dim(example_run$samples$original), c(total_length, 3L))
  })

  niter <- example_run$niter
  it("Stores weights as list", {
    expect_named(
      example_run$weights,
      c("id", "evaluations", "log_lik", "log_weight", "imp_weight", "birth_lik")
    )
    expect_length(example_run$weights$id, total_length)
    expect_type(example_run$weights$id, "integer")
    expect_type(example_run$weight$evaluations, "integer")
    expect_type(example_run$weight$log_lik, "double")
    expect_type(example_run$weight$log_weight, "double")
    expect_equal(sum(example_run$weight$imp_weight), 1)
    expect_type(example_run$weight$birth_lik, "double")
  })
  expect_snapshot(example_run, transform = \(x) gsub("\\d+", "#", x))
})

describe("summary.ernest_run returns expected structure and values", {
  set.seed(42)
  smry <- summary(example_run)
  it("has the correct meta-info", {
    expect_s3_class(smry, c("summary.ernest_run"))
    expect_true(is.list(smry))
    expect_equal(smry$nlive, 1000L)
    expect_equal(smry$niter, example_run$niter)
    expect_equal(smry$neval, example_run$neval)
    expect_equal(smry$log_evidence, example_run$log_evidence)
    expect_equal(smry$log_evidence_err, example_run$log_evidence_err)
    expect_equal(smry$information, example_run$information)
    expect_equal(smry$seed, attr(example_run, "seed"))
  })

  it("has the correct meta-info", {
    expect_s3_class(smry, c("summary.ernest_run"))
    expect_true(is.list(smry))
    expect_equal(smry$niter, example_run$niter)
    expect_equal(smry$neval, example_run$neval)
    expect_equal(smry$log_evidence, example_run$log_evidence)
    expect_equal(smry$log_evidence_err, example_run$log_evidence_err)
    expect_equal(smry$information, example_run$information)
    expect_equal(smry$seed, attr(example_run, "seed"))
  })

  it("has the expected MLE", {
    max_idx <- which.max(example_run$weights$log_lik)
    max_loglik <- example_run$weights$log_lik[max_idx]
    expect_named(smry$mle, c("log_lik", "original", "unit_cube"))
    expect_equal(smry$mle$log_lik, max_loglik)
  })

  it("has the expected posterior", {
    expect_identical(
      dim(smry$reweighted_samples),
      c(example_run$niter + example_run$nlive, 3L)
    )
    expect_named(
      smry$posterior,
      c("variable", "mean", "sd", "median", "q15", "q85")
    )
    expect_snapshot_value(smry$posterior, style = "json2", tolerance = 0.1)
  })
})
