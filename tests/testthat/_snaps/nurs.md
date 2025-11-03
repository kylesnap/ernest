# nurs can be called by user

    Code
      nurs(adaptive_scale = -0.1)
    Condition
      Error in `new_nurs()`:
      ! `adaptive_scale` must be a number between 0 and 1 or `NULL`, not the number -0.1.

---

    Code
      nurs(adaptive_scale = 1.1)
    Condition
      Error in `new_nurs()`:
      ! `adaptive_scale` must be a number between 0 and 1 or `NULL`, not the number 1.1.

---

    Code
      nurs(fixed_scale = 0)
    Condition
      Error in `new_nurs()`:
      ! `fixed_scale` must be larger than 0.

---

    Code
      nurs(adaptive_scale = 0.1, fixed_scale = 0.3)
    Condition
      Error in `nurs()`:
      ! Exactly one of `adaptive_scale` or `fixed_scale` must be supplied.

---

    Code
      nurs(adaptive_scale = 0.1, steps = 0)
    Condition
      Error in `new_nurs()`:
      ! `steps` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      nurs(adaptive_scale = 0.1, max_orbits = 0)
    Condition
      Error in `new_nurs()`:
      ! `max_orbits` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      default
    Message
      <nurs> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      No. Steps: 3
      Scaling Method: Adaptive
      Current Scale: 0.1000
      

# nurs class: Can call propose

    Code
      as.list(lrps$cache)
    Output
      $epsilon
      [1] 0.01
      
      $n_call
      [1] 104
      
      $n_cmp
      [1] 36
      
      $n_accept
      [1] 3
      

# nurs class: Can be updated

    Code
      obj
    Message
      <nurs> lrps:
      No. Dimensions: 2
      No. Calls Since Update: 0
      No. Steps: 3
      Scaling Method: Fixed
      Current Scale: 0.01000
      

# nurs with fixed scale: Can call propose

    Code
      as.list(lrps$cache)
    Output
      $epsilon
      [1] 0.01
      
      $n_call
      [1] 104
      
      $n_cmp
      [1] 36
      
      $n_accept
      [1] 3
      

# Errors in distance recalculations are handled

    Code
      new_obj <- update_lrps(obj, unit = matrix())
    Condition
      Warning:
      Failed to update lattice scale.
      Caused by error in `update_lrps()`:
      ! `epsilon` must be a finite scalar double, not NA.

