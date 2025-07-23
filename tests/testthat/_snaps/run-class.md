# ernest_result returns as expected

    Code
      result
    Output
      An <ernest_run>: 500 points x 100 iter x 107 lik. calls
      > Log. Evidence: -4.529 ± 0.485

# Runs can continue after one call

    Code
      result2
    Output
      An <ernest_run>: 500 points x 300 iter x 400 lik. calls
      > Log. Evidence: -4.594 ± 0.429

# Runs can continue after two calls

    Code
      result3
    Output
      An <ernest_run>: 500 points x 1000 iter x 16651 lik. calls
      > Log. Evidence: -4.687 ± 0.265

# Summary method returns

    Code
      summary(result3)
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 1000
      No. Lik. Calls: 16651
      Log. Evidence: -4.687 (± 0.2648)

