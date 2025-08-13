# Informative error when prior or log. lik. fails completely.

    Can't create live points.
    Caused by error in `private$log_lik()`:
    ! Bad Likelihood Job!

---

    Can't create live points.
    Caused by error in `private$prior_fn()`:
    ! Bad prior job!

# check_live_set catches problems in the live_env

    `unit` must be a matrix, not a double vector.

---

    `unit` must have dimensions 500 x 2.
    x `unit` instead has dimensions 250 x 4

---

    Problem at the 5th row of `unit`.
    Caused by error in `check_matrix()`:
    ! `unit` must not contain missing or `NaN` values.

---

    Problem at the 500th row of `unit`.
    Caused by error in `check_matrix()`:
    ! `unit` must respect the lower boundary (0).

---

    `log_lik` must be a double vector of size 500, not 499.

---

    `log_lik` must not contain missing, `NaN`, or `Inf` values.

---

    `log_lik` must not contain missing, `NaN`, or `Inf` values.

---

    `log_lik` must contain a range of likelihood values.
    x `log_lik` currently contains one unique value (-10).

---

    `log_lik` may contain a likelihood plateau; proceed with caution.
    ! Only 250/500 likelihood values are unique.

# compile method initializes live points

    Code
      sampler
    Output
      An <ernest_sampler>: 500 points, 2 variables. 

