# nurs can be called by user

    Code
      no_underrun(adaptive_scale = -0.1)
    Condition
      Error in `new_no_underrun()`:
      ! `adaptive_scale` must be a number between 0 and 1 or `NULL`, not the number -0.1.

---

    Code
      no_underrun(adaptive_scale = 1.1)
    Condition
      Error in `new_no_underrun()`:
      ! `adaptive_scale` must be a number between 0 and 1 or `NULL`, not the number 1.1.

---

    Code
      no_underrun(fixed_scale = 0)
    Condition
      Error in `new_no_underrun()`:
      ! `fixed_scale` must be larger than 0.

---

    Code
      no_underrun(adaptive_scale = 0.1, fixed_scale = 0.3)
    Condition
      Error in `no_underrun()`:
      ! Exactly one of `adaptive_scale` or `fixed_scale` must be supplied.

---

    Code
      no_underrun(adaptive_scale = 0.1, steps = 0)
    Condition
      Error in `new_no_underrun()`:
      ! `steps` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      no_underrun(adaptive_scale = 0.1, max_orbits = 0)
    Condition
      Error in `new_no_underrun()`:
      ! `max_orbits` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      default
    Message
      ernest LRPS method <no_underrun/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * No. Steps: 3
      * Scaling Method: Adaptive
      * Current Scale: 0.1000

# nurs class: Can be updated

    Code
      obj
    Message
      ernest LRPS method <no_underrun/ernest_lrps>
      • Dimensions: 2
      • No. Log-Lik Calls: 0
      • No. Steps: 3
      • Scaling Method: Fixed
      • Current Scale: 0.01000

---

    Code
      obj
    Message
      ernest LRPS method <no_underrun/ernest_lrps>
      • Dimensions: 2
      • No. Log-Lik Calls: 0
      • No. Steps: 3
      • Scaling Method: Fixed
      • Current Scale: 0.01000

# nurs can provide good results

    c(n_iter = 4836, log_evidence = -6.64802524203653, log_evidence_var = 0.0149694817664634
    )

---

    c(n_iter = 5789, log_evidence = -8.85468489468861, log_evidence_var = 0.0187579698879188
    )

---

    c(n_iter = 4973, log_evidence = 236.078388543111, log_evidence_var = 0.0139944849858656
    )

# Errors in distance recalculations are handled

    Code
      new_obj <- update_lrps(obj, unit = matrix())
    Condition
      Warning:
      Failed to update lattice scale.
      Caused by error in `update_lrps()`:
      ! `epsilon` must be a finite scalar double, not NA.

