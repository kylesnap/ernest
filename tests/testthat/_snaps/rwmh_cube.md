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

# rwmh_cube can provide good results

    c(n_iter = 4928, log_evidence = -6.83131400467416, log_evidence_var = 0.0155273926462523
    )

---

    c(n_iter = 5806, log_evidence = -8.88911918431688, log_evidence_var = 0.0190070444163182
    )

---

    c(n_iter = 4944, log_evidence = 236.136248848103, log_evidence_var = 0.013928242406257
    )

