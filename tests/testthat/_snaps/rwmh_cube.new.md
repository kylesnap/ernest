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

    c(n_iter = 4812, log_evidence = -6.60031281386334, log_evidence_var = 0.0149144257575914
    )

---

    c(n_iter = 5831, log_evidence = -8.9412997048161, log_evidence_var = 0.018999212939008
    )

---

    c(n_iter = 5035, log_evidence = 235.954489352236, log_evidence_var = 0.0143108426123302
    )

