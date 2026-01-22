# create_live / gives informative error when unit_log_fn fails completely

    Code
      create_live(bad_lik, 10)
    Condition
      Error:
      ! Error when creating live points.
      Caused by error in `unit_log_fn()`:
      ! Bad Job!

# check_live_set / errors if unit is not a matrix of correct shape

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `unit` must be a matrix, not a double vector.

# check_live_set / errors if unit matrix has wrong dimensions

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `unit` must have 500 rows, not 250.

# check_live_set / errors if unit matrix contains NaN

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `unit` must contain no nonfinite values.

# check_live_set / errors if log_lik is too short

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `log_lik` must be a double vector with length 500.

# check_live_set / errors if log_lik contains NaN

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `log_lik` must contain only finite values or `-Inf`.

# check_live_set / errors if log_lik contains Inf

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `log_lik` must contain only finite values or `-Inf`.

# check_live_set / errors if log_lik is a plateau (all values identical)

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `log_lik` must contain a range of likelihood values.
      x `log_lik` currently contains one unique value (-10).

# check_live_set / warns if log_lik has repeated values but not all identical

    Code
      check_live_set(sampler)
    Condition
      Warning:
      `log_lik` may contain a likelihood plateau; proceed with caution.
      ! Only 250/500 likelihood values are unique.

# check_live_set / errors if birth_lik vector is wrong

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `birth_lik` vector cannot be missing from the `run_env` environment.

---

    Code
      check_live_set(sampler)
    Condition
      Error:
      ! `birth_lik` vector cannot be missing from the `run_env` environment.

# compile / initializes live points

    Code
      sampler
    Message
      Nested sampling run specification:
      * Live points: 500
      * Sampling method: 25-step random walk sampling (acceptance target = 50.0%)
      * Prior: uniform prior distribution with 2 dimensions (A and B)

