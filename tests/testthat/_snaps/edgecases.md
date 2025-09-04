# Zero-length likelihood fails

    <ernest_sampler> cannot compile.
    Caused by error in `compile()`:
    ! log-lik. values must be scalar doubles, not an empty numeric vector.

# Zero-length prior fails

    `n_dim` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Can't validate `fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! `prior$fn(x)` must have size 1, not size 0.

# Fails on character types

    Can't validate `fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! Can't convert `y` <character> to <double>.

---

    <ernest_sampler> cannot compile.
    Caused by error in `compile()`:
    ! log-lik. values must be scalar doubles, not the string "U".

# Fails on complex types

    Can't validate `fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! Can't convert `y` <complex> to <double>.

---

    <ernest_sampler> cannot compile.
    Caused by error in `compile()`:
    ! log-lik. values must be scalar doubles, not the complex number 0+0.3i.

# Missing values in the prior

    Can't validate `fn` as a valid prior.
    Caused by error in `FUN()`:
    ! Priors must only contain finite values, not 0.914806043496355 and NaN.

# Missing values in the log-likelihood

    <ernest_sampler> cannot compile.
    Caused by error in `compile()`:
    ! log-lik. values must be either finite or `-Inf`, not NA.

---

    <ernest_sampler> threw a warning during compilation
    Caused by warning in `compile()`:
    ! Replacing `NA` with `-Inf`.

# Ernest fails when ll is flat to begin with

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! `log_lik` must contain a range of likelihood values.
    x `log_lik` currently contains one unique value (0).

# Ernest halts and warns when ll becomes flat during a run

    Code
      generate(sampler, seed = 42L)
    Condition
      Warning in `compile()`:
      `log_lik` may contain a likelihood plateau; proceed with caution.
      ! Only 130/500 likelihood values are unique.
      Warning:
      Stopping run due to a likelihood plateau at 0.
    Output
      Nested sampling run <ernest_run/ernest_sampler>
      No. Points: 500
      
      -- Sampling Method 
      * Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1
      
      -- Results 
      No. Iterations: 149
      No. Calls: 179
      Log. Evidence: -0.2973 (± 0.0636)

