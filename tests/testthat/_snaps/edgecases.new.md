# Ernest halts and warns when ll becomes flat during a run

    Code
      generate(sampler)
    Condition
      Warning in `compile()`:
      `log_lik` may contain a likelihood plateau; proceed with caution.
      ! Only 144/500 likelihood values are unique.
      Warning:
      Stopping run due to a likelihood plateau at 0.
    Output
      An <ernest_run>: 500 points, 2 variables
      * 159 iterations, 178 likelihood calls
      * Log. Evidence ≈ -0.319 (± 0.066)

