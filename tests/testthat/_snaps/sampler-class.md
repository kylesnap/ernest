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
      An <ernest_sampler>: 500 points x 99 iter. x 2475 lik. calls

---

    Code
      summary(result)
    Output
      
      -- Nested Sampling Results from <ernest_run> -----------------------------------
      No. Points: 500
      No. Iterations: 99
      No. Lik. Calls: 2475
      Log. Evidence: -4.774 (± 0.542)
      
      -- Weighted Posterior Distribution 
      # A tibble: 2 x 7
        variable          mean median    sd   mad     q5   q95
        <chr>            <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>
      1 Uniform(-5, 5)   -1.09  -1.13 0.960 0.879 -2.87  0.456
      2 Uniform(-5, 5).1  1.08   1.02 1.04  1.14  -0.738 2.80 

