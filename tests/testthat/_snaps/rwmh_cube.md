# rwmh_cube returns correct class and structure

    Code
      obj
    Output
      random walk in unit cube LRPS <rwmh_cube/ernest_lrps>
      
      No. Dimensions: Uninitialized
      No. Calls Since Update: 0
      No. Accepted Since Update: 0
      Current Step Size: 1.000

# new_rwmh_cube: errors on invalid arguments

    Code
      new_rwmh_cube(fn, 2L, target_acceptance = 0)
    Condition
      Error in `new_rwmh_cube()`:
      ! `target_acceptance` must be at least 1/25.

---

    Code
      new_rwmh_cube(fn, 2L, target_acceptance = 1)
    Condition
      Error in `new_rwmh_cube()`:
      ! `target_acceptance` must be smaller than 1.

---

    Code
      new_rwmh_cube(fn, 2L, steps = 1)
    Condition
      Error in `new_rwmh_cube()`:
      ! `steps` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      new_rwmh_cube(fn, 2L, cov_fn = "fizz")
    Condition
      Error in `new_rwmh_cube()`:
      ! `cov_fn` must be an R function or `NULL`, not the string "fizz".

# propose.rwmh_cube: proposes a single new point

    Code
      rwcube
    Output
      random walk in unit cube LRPS <rwmh_cube/ernest_lrps>
      
      No. Dimensions: 2
      No. Calls Since Update: 0
      No. Accepted Since Update: 0
      Current Step Size: 1.000

# update_lrps.rwmh_cube: warns when chol_cov can't be calculated

    Code
      update_lrps(rwcube, bad_live)
    Condition
      Warning:
      Using an identity matrix after failed.
      Caused by error in `chol.default()`:
      ! the leading minor of order 1 is not positive
    Output
      random walk in unit cube LRPS <rwmh_cube/ernest_lrps>
      
      No. Dimensions: 2
      No. Calls Since Update: 0
      No. Accepted Since Update: 0
      Current Step Size: 0.7023

