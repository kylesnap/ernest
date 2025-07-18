# ernest_sampler initializes correctly

    Code
      sampler
    Message
      An <ernest_sampler>: 500 points x 0 iter. x 0 lik. calls

# compile method initializes live points

    Code
      sampler
    Message
      An <ernest_sampler>: 500 points x 0 iter. x 0 lik. calls

# generate method performs sampling

    Code
      sampler
    Message
      An <ernest_sampler>: 500 points x 99 iter. x 114 lik. calls

---

    Code
      summary(result)
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 99
      No. Lik. Calls: 114
      Log. Evidence: -4.668 (± 0.5263)
      
      -- Weighted Posterior Distribution 
      # A tibble: 2 x 7
        variable           mean median    sd   mad      q5   q95
        <chr>             <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl>
      1 Uniform(-5, 5)   -0.992 -0.785 0.981 1.09  -2.56   0.645
      2 Uniform(-5, 5).1  1.06   0.884 0.897 0.661 -0.0764 2.68 

