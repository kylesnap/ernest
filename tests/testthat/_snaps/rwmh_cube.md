# new_rwmh_cube errors on invalid arguments

    `target_acceptance` must be at least 1/25.

---

    `target_acceptance` must be smaller than 1.

---

    `steps` must be a whole number larger than or equal to 2, not the number 1.

---

    `cov_fn` must be an R function or `NULL`, not the string "fizz".

# rwmh_cube returns correct class and structure

    Code
      obj
    Output
      Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      No. Dimensions: Uninitialized
      No. Calls Since Update: 0
      No. Accepted Since Update: 0
      Current Step Size: 1

# propose.rwmh_cube proposes a single new point

    Code
      rwcube
    Output
      Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      No. Dimensions: 2
      No. Calls Since Update: 0
      No. Accepted Since Update: 0
      Current Step Size: 1

# propose.rwmh_cube proposes a single point under a likelihood constraint

    Code
      rwcube
    Output
      Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      No. Dimensions: 2
      No. Calls Since Update: 25
      No. Accepted Since Update: 0
      Current Step Size: 1

# update_lrps warns when chol_cov can't be calculated

    Using an identity matrix after failed.
    Caused by error in `chol.default()`:
    ! the leading minor of order 1 is not positive

