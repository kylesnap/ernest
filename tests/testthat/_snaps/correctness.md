# Gaussian shells: 2D

    Code
      smry
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 3445
      No. Lik. Calls: 71906
      Log. Evidence: -1.8 (± 0.08682)
      
      -- Weighted Posterior Distribution 
      # A tibble: 2 x 7
        variable           mean median    sd   mad    q5   q95
        <chr>             <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
      1 Uniform(-6, 6)   1.63    1.65   2.30  2.78 -1.94  5.36
      2 Uniform(-6, 6).1 0.0830  0.195  1.42  2.03 -1.98  1.97

# Gaussian shells: 5D

    Code
      smry
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 5357
      No. Lik. Calls: 119107
      Log. Evidence: -5.637 (± 0.1302)
      
      -- Weighted Posterior Distribution 
      # A tibble: 5 x 7
        variable              mean  median    sd   mad    q5   q95
        <chr>                <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>
      1 Uniform(-6, 6)    1.65      1.51   2.00   2.68 -1.30  4.69
      2 Uniform(-6, 6).1 -0.000534 -0.0149 0.927  1.09 -1.50  1.52
      3 Uniform(-6, 6).2  0.0636    0.0454 0.895  1.06 -1.37  1.53
      4 Uniform(-6, 6).3  0.0110    0.0243 0.891  1.02 -1.45  1.45
      5 Uniform(-6, 6).4 -0.0338   -0.0669 0.897  1.05 -1.47  1.45

# Gaussian shells: 10D

    Code
      smry
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 9955
      No. Lik. Calls: 234129
      Log. Evidence: -14.84 (± 0.1974)
      
      -- Weighted Posterior Distribution 
      # A tibble: 10 x 7
         variable             mean   median    sd   mad     q5   q95
         <chr>               <dbl>    <dbl> <dbl> <dbl>  <dbl> <dbl>
       1 Uniform(-6, 6)    1.50     0.846   1.89  2.38  -0.955 4.27 
       2 Uniform(-6, 6).1 -0.0491  -0.0609  0.662 0.735 -1.13  1.05 
       3 Uniform(-6, 6).2 -0.0247  -0.0195  0.614 0.622 -1.04  0.996
       4 Uniform(-6, 6).3  0.0170  -0.00394 0.645 0.683 -1.02  1.10 
       5 Uniform(-6, 6).4  0.00431  0.0187  0.640 0.690 -1.08  1.03 
       6 Uniform(-6, 6).5 -0.0211  -0.0238  0.652 0.692 -1.09  1.03 
       7 Uniform(-6, 6).6  0.0242   0.00298 0.645 0.684 -1.04  1.09 
       8 Uniform(-6, 6).7 -0.00671 -0.00999 0.648 0.684 -1.10  1.07 
       9 Uniform(-6, 6).8 -0.00555 -0.0142  0.652 0.703 -1.11  1.09 
      10 Uniform(-6, 6).9 -0.00840  0.0131  0.650 0.697 -1.14  1.02 

# Eggbox

    Code
      smry
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 5001
      No. Lik. Calls: 110328
      Log. Evidence: 236 (± 0.1262)
      
      -- Weighted Posterior Distribution 
      # A tibble: 2 x 7
        variable              mean median    sd   mad     q5   q95
        <chr>                <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>
      1 Uniform(0, 31.416)    16.4   18.8  8.63  9.34 0.0771  25.3
      2 Uniform(0, 31.416).1  14.9   12.6  8.76  9.46 0.106   25.3

