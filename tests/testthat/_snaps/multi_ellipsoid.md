# multi_ellipsoid can be called by user

    Code
      multi_ellipsoid(enlarge = 0.5)
    Condition
      Error in `multi_ellipsoid()`:
      ! `enlarge` must be a number larger than or equal to 1, not the number 0.5.

---

    Code
      multi_ellipsoid(enlarge = 1)
    Condition
      Warning:
      `enlarge` is set to 1, which is not recommended.
    Message
      ernest LRPS method <multi_ellipsoid/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * No. Ellipsoids: 0
      * Total Log Volume: -Inf
      * Enlargement: 1

---

    Code
      default
    Message
      ernest LRPS method <multi_ellipsoid/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * No. Ellipsoids: 0
      * Total Log Volume: -Inf
      * Enlargement: 1.25

# multi_ellipsoid can provide good results

    c(n_iter = 4860, log_evidence = -6.69609161636962, log_evidence_var = 0.0151598675440093
    )

---

    c(n_iter = 5745, log_evidence = -8.76435668711012, log_evidence_var = 0.0185549011508603
    )

---

    c(n_iter = 5103, log_evidence = 235.817113800345, log_evidence_var = 0.0145077406271789
    )

# update throws a warning when the points are all identical

    Code
      update_lrps(obj, live)
    Condition
      Warning:
      Multi-ellipsoid fitting returned an error code (1).
    Message
      ernest LRPS method <multi_ellipsoid/ernest_lrps>
      * Dimensions: 2
      * No. Log-Lik Calls: 0
      * No. Ellipsoids: 1
      * Total Log Volume: 0.4516
      * Enlargement: 1

