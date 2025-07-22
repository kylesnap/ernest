# Gaussian shells: 2D

    Code
      smry
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 3425
      No. Lik. Calls: 77403
      Log. Evidence: -1.76 (± 0.08651)
      
      -- Weighted Posterior Distribution 
      # A tibble: 2 x 7
        variable    mean median    sd   mad    q5   q95
        <chr>      <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
      1 x         1.58    1.67   2.24  2.57 -1.87  5.38
      2 y        -0.0408 -0.152  1.43  2.12 -1.99  1.99

# Gaussian shells: 5D

    Code
      smry
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 5321
      No. Lik. Calls: 124703
      Log. Evidence: -5.557 (± 0.1303)
      
      -- Weighted Posterior Distribution 
      # A tibble: 5 x 7
        variable              mean   median    sd   mad    q5   q95
        <chr>                <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl>
      1 Uniform(-6, 6)    1.75      1.80    1.95  2.58  -1.20  4.64
      2 Uniform(-6, 6).1 -0.0768   -0.0657  0.903 1.07  -1.57  1.36
      3 Uniform(-6, 6).2 -0.00436   0.00301 0.886 1.02  -1.53  1.37
      4 Uniform(-6, 6).3  0.000904  0.0244  0.917 1.05  -1.51  1.49
      5 Uniform(-6, 6).4 -0.0374   -0.0327  0.869 0.943 -1.47  1.40

# Gaussian shells: 10D

    Code
      smry
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 9726
      No. Lik. Calls: 234702
      Log. Evidence: -14.53 (± 0.1938)
      
      -- Weighted Posterior Distribution 
      # A tibble: 10 x 7
         variable             mean   median    sd   mad     q5   q95
         <chr>               <dbl>    <dbl> <dbl> <dbl>  <dbl> <dbl>
       1 Uniform(-6, 6)    2.16     2.92    1.82  1.79  -0.736  4.45
       2 Uniform(-6, 6).1  0.00809  0.0373  0.652 0.682 -1.09   1.08
       3 Uniform(-6, 6).2  0.0175   0.0325  0.627 0.665 -1.03   1.05
       4 Uniform(-6, 6).3  0.0158   0.0406  0.646 0.716 -1.03   1.08
       5 Uniform(-6, 6).4  0.00961  0.00420 0.659 0.704 -1.07   1.10
       6 Uniform(-6, 6).5  0.0289   0.0402  0.622 0.640 -1.04   1.05
       7 Uniform(-6, 6).6 -0.00356 -0.0139  0.671 0.714 -1.09   1.15
       8 Uniform(-6, 6).7  0.0100   0.0316  0.660 0.690 -1.12   1.07
       9 Uniform(-6, 6).8  0.0212   0.0202  0.651 0.676 -1.09   1.11
      10 Uniform(-6, 6).9  0.0100   0.0147  0.635 0.676 -1.03   1.04

# Eggbox

    Code
      smry
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 5014
      No. Lik. Calls: 117352
      Log. Evidence: 236 (± 0.1261)
      
      -- Weighted Posterior Distribution 
      # A tibble: 2 x 7
        variable              mean median    sd   mad     q5   q95
        <chr>                <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>
      1 Uniform(0, 31.416)    15.5   12.7  9.77  9.66 0.0555  31.3
      2 Uniform(0, 31.416).1  14.9   12.6  9.97  9.58 0.0445  31.3

