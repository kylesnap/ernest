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
      An <ernest_run>: 500 points x 300 iter x 3402 lik. calls
      > Log. Evidence: -4.574 ± 0.431

# Runs can continue after two calls

    Code
      result3
    Output
      An <ernest_run>: 500 points x 1000 iter x 20902 lik. calls
      > Log. Evidence: -4.544 ± 0.245

# Summary method returns

    Code
      summary(result3)
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 1000
      No. Lik. Calls: 20902
      Log. Evidence: -4.544 (± 0.2446)
      
      -- Weighted Posterior Distribution 
      # A tibble: 2 x 7
        variable           mean median    sd   mad     q5   q95
        <chr>             <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>
      1 Uniform(-5, 5)   -0.989 -0.968 0.971 0.980 -2.60  0.601
      2 Uniform(-5, 5).1  0.993  1.02  0.927 0.916 -0.498 2.54 

