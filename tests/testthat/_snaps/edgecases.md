# Zero-length likelihood fails

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! log-lik. values must be single scalars, not vectors of size 0.

# Zero-length prior fails

    `n_dim` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    `fn` must return a vector of length 1, not one of length 0.

# Fails on character types

    `fn` must return a numeric vector, not a character vector.

---

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! Can't convert `log-lik.` <character> to <double>.

# Fails on complex types

    `fn` must return a numeric vector, not a complex vector.

---

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! Can't convert `log-lik.` <complex> to <double>.

# Missing values in the prior

    `fn` failed a sanity check.
    x Input: 0.2861, 0.8198
    x Output: NaN, 0.8198
    Caused by error in `create_prior()`:
    ! `fn` must return vectors that only contain finite values.

# Missing values in the log-likelihood

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! log-lik. values must be either finite or `-Inf`, not NA.

---

    <ernest_sampler> threw a warning during compilation
    Caused by warning:
    ! Replacing `NA` with `-Inf`.

# Ernest fails when ll is flat to begin with

    `lower` must be strictly smaller than `upper`.

# Ernest halts and warns when ll becomes flat during a run

    Code
      generate(sampler, seed = 42L)
    Condition
      Warning in `compile()`:
      `log_lik` may contain a likelihood plateau; proceed with caution.
      ! Only 130/500 likelihood values are unique.
      Warning:
      Stopping run due to a likelihood plateau at 0.0000.
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "random walk in unit cube LRPS <rwmh_cube/ernest_lrps>"
      [2] ""                                                     
      [3] "No. Dimensions: 2"                                    
      [4] "Current Step Size: 1.000"                             
      
      -- Results 
      No. Iterations: 149
      No. Calls: 179
      Log. Evidence: -0.2973 (± 0.06362)

