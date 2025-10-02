# create_live: gives informative error when prior or log. lik. fails completely

    Code
      create_live(bad_lik, 10)
    Condition
      Error:
      ! Bad Likelihood Job!

# check_live_set: errors if unit is not a matrix of correct shape

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `unit` must be a double matrix, not a double vector.

# check_live_set: errors if unit matrix has wrong dimensions

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `unit` must have dimensions 500 x 2.
      x `unit` instead has dimensions 250 x 4

# check_live_set: errors if unit matrix contains NaN

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `unit` must not contain missing or `NaN` values.

# check_live_set: errors if log_lik is too short

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `log_lik` must be a double vector with length 500.

# check_live_set: errors if log_lik contains NaN

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `log_lik` must contain only finite values or `-Inf`.

# check_live_set: errors if log_lik contains Inf

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `log_lik` must contain only finite values or `-Inf`.

# check_live_set: errors if log_lik is a plateau (all values identical)

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `log_lik` must contain a range of likelihood values.
      x `log_lik` currently contains one unique value (-10).

# check_live_set: warns if log_lik has repeated values but not all identical

    `log_lik` may contain a likelihood plateau; proceed with caution.
    ! Only 250/500 likelihood values are unique.

# check_live_set: errors if birth vector is wrong

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `birth` vector cannot be missing from the `run_env` environment.

---

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `birth` vector cannot be missing from the `run_env` environment.

# compile: initializes live points

    Code
      sampler
    Output
      nested sampling specification <ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      * random walk in unit cube LRPS <rwmh_cube/ernest_lrps>
      * 
      * No. Dimensions: 2
      * Current Step Size: 1.000

