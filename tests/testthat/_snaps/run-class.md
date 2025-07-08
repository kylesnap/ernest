# ernest_result returns as expected

    Code
      result
    Output
      An <ernest_run>: 500 points x 100 iter x 2500 lik. calls
      > Log. Evidence: -4.607 ± 0.5

# Runs can continue after one call

    Code
      result2
    Output
      An <ernest_run>: 500 points x 300 iter x 7500 lik. calls
      > Log. Evidence: -4.66 ± 0.443

# Runs can continue after two calls

    Code
      result3
    Output
      An <ernest_run>: 500 points x 1000 iter x 25000 lik. calls
      > Log. Evidence: -4.645 ± 0.258

# Summary method returns

    Code
      summary(result3)
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 1000
      No. Lik. Calls: 25000
      Log. Evidence: -4.645 (± 0.2576)
      
      -- Weighted Posterior Distribution 
      # A tibble: 2 x 7
        variable           mean median    sd   mad     q5   q95
        <chr>             <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>
      1 Uniform(-5, 5)   -0.942 -0.898 0.972 0.965 -2.60  0.642
      2 Uniform(-5, 5).1  1.01   1.04  0.960 0.970 -0.587 2.56 

