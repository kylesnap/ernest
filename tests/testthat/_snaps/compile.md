# Informative error when prior or log. lik. fails completely.

    Bad Likelihood Job!

---

    Bad Prior Job!

# check_live_set catches problems in the live_env

    `unit` must be a double matrix, not a double vector.

---

    `unit` must have dimensions 500 x 2.
    x `unit` instead has dimensions 250 x 4

---

    `unit` must not contain missing or `NaN` values.

---

    `log_lik` must be a double vector with length 500.

---

    missing value where TRUE/FALSE needed

---

    `log_lik` must contain only finite values or `-Inf`.

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
      Nested sampling specification <ernest_sampler>
      No. Points: 500
      
      -- Sampling Method 
      * Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1

