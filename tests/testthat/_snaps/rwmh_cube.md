# rwmh_cube can be called by user

    Code
      rwmh_cube(steps = 1)
    Condition
      Error in `new_rwmh_cube()`:
      ! `steps` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      rwmh_cube(target_acceptance = 0.01)
    Condition
      Error in `new_rwmh_cube()`:
      ! `target_acceptance` must be at least 1/25.

---

    Code
      rwmh_cube(target_acceptance = 1.1)
    Condition
      Error in `new_rwmh_cube()`:
      ! `target_acceptance` must be smaller than 1.

---

    Code
      default
    Message
      ernest LRPS method <rwmh_cube/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * No. Accepted Proposals: 0
      * No. Steps: 25
      * Target Acceptance: 0.5
      * Step Size: 1.000

# rwmh_cube class: Can call propose

    Code
      as.list(lrps$cache)
    Output
      $epsilon
      [1] 1
      
      $n_call
      [1] 25
      
      $n_accept
      [1] 3
      

# rwmh_cube can provide good results

    Code
      result
    Message
      nested sampling results <ernest_run/ernest_sampler>
      * No. Points: 500
      * LRPS Method: rwmh_cube
      --------------------------------------------------------------------------------
      * No. Iterations: 4868
      * No. Calls: 107402
      * Log. Evidence: -6.713 (± 0.1236)

---

    Code
      result
    Message
      nested sampling results <ernest_run/ernest_sampler>
      * No. Points: 500
      * LRPS Method: rwmh_cube
      --------------------------------------------------------------------------------
      * No. Iterations: 5774
      * No. Calls: 130127
      * Log. Evidence: -8.824 (± 0.1370)

---

    Code
      result
    Message
      nested sampling results <ernest_run/ernest_sampler>
      * No. Points: 500
      * LRPS Method: rwmh_cube
      --------------------------------------------------------------------------------
      * No. Iterations: 4899
      * No. Calls: 109001
      * Log. Evidence: 236.2 (± 0.1173)

