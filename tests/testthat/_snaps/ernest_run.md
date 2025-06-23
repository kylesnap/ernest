# ernest_result returns as expected

    Code
      result
    Output
      
      -- Ernest Nested Sampling Run --------------------------------------------------
      No. Live Points: 500
      No. Iterations: 100
      No. Lik. Calls: 2500
      Log. Evidence (± Err.): -4.731 (± 0.5246)

# Runs can continue after one call

    Code
      result2
    Output
      
      -- Ernest Nested Sampling Run --------------------------------------------------
      No. Live Points: 500
      No. Iterations: 300
      No. Lik. Calls: 7500
      Log. Evidence (± Err.): -4.859 (± 0.4803)

# Runs can continue after two calls

    Code
      result3
    Output
      
      -- Ernest Nested Sampling Run --------------------------------------------------
      No. Live Points: 500
      No. Iterations: 1000
      No. Lik. Calls: 25000
      Log. Evidence (± Err.): -4.781 (± 0.279)

# Summary method returns

    Code
      summary(result3)
    Output
      
      -- Ernest Nested Sampling Run Summary ------------------------------------------
      No. Points: 500
      No. Iterations: 1000
      No. Lik. Calls: 25000
      Log. Evidence (± Err.): -4.781 (± 0.279)

