# slice can be called by user

    Code
      slice_rectangle(enlarge = 0.5)
    Condition
      Error in `slice_rectangle()`:
      ! `enlarge` must be a number larger than or equal to 1 or `NA`, not the number 0.5.

---

    Code
      slice_rectangle(enlarge = NA)
    Message
      ernest LRPS method <slice_rectangle/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Centre: Undefined
      * Enlargement: Disabled

---

    Code
      default
    Message
      ernest LRPS method <slice_rectangle/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Centre: Undefined
      * Enlargement: 1

# slice class: Can be constructed with new_

    Code
      obj
    Message
      ernest LRPS method <slice_rectangle/ernest_lrps>
      * Dimensions: 2
      * No. Log-Lik Calls: 0
      * Centre: 0.5000, 0.5000
      * Enlargement: 1.25

# slice can provide good results

    c(n_iter = 4862, log_evidence = -6.70118162708326, log_evidence_var = 0.0151335824551731
    )

---

    c(n_iter = 5811, log_evidence = -8.89723183787065, log_evidence_var = 0.0188952475142511
    )

---

    c(n_iter = 4992, log_evidence = 236.04038549352, log_evidence_var = 0.0140733117814586
    )

