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

    c(n_iter = 4868, log_evidence = -6.7129628504232, log_evidence_var = 0.0152651070257088
    )

---

    c(n_iter = 5774, log_evidence = -8.82355135828063, log_evidence_var = 0.0187671313329496
    )

---

    c(n_iter = 4899, log_evidence = 236.225012283386, log_evidence_var = 0.013758657893653
    )

