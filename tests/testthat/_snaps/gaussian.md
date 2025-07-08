# Gaussian case works with prior function

    Code
      sampler
    Message
      An <ernest_sampler>: 500 points x 0 iter. x 0 lik. calls

---

    Code
      result
    Output
      An <ernest_run>: 500 points x 5833 iter x 145825 lik. calls
      > Log. Evidence: -8.939 ± 0.138

---

    Code
      sum
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 5833
      No. Lik. Calls: 145825
      Log. Evidence: -8.939 (± 0.1382)
      
      -- Weighted Posterior Distribution 
      # A tibble: 3 x 7
        variable    mean  median    sd   mad     q5   q95
        <chr>      <dbl>   <dbl> <dbl> <dbl>  <dbl> <dbl>
      1 x1       -1.05   -1.03   0.929 0.915 -2.63  0.435
      2 x2       -0.0526 -0.0140 0.937 0.959 -1.65  1.41 
      3 x3        0.952   0.986  0.929 0.930 -0.608 2.40 

