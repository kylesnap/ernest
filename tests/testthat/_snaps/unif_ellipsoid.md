# unif_ellipsoid can be called by user

    Code
      unif_ellipsoid(enlarge = 0.5)
    Condition
      Error in `unif_ellipsoid()`:
      ! `enlarge` must be a number larger than or equal to 1, not the number 0.5.

---

    Code
      unif_ellipsoid(enlarge = 1)
    Message
      ! `enlarge` is set to 1.0, which is not recommended.
      ernest LRPS method <unif_ellipsoid/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Center: Undefined
      * Log Volume: -Inf
      * Enlargement: 1

---

    Code
      default
    Message
      ernest LRPS method <unif_ellipsoid/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Center: Undefined
      * Log Volume: -Inf
      * Enlargement: 1.25

# update throws a warning when the points are all identical

    Code
      update_lrps(obj, live)
    Condition
      Warning:
      Ellipsoid fitting returned an error code (1).
    Message
      ernest LRPS method <unif_ellipsoid/ernest_lrps>
      * Dimensions: 2
      * No. Log-Lik Calls: 0
      * Center: 0.5000, 0.5000
      * Log Volume: 0.4516
      * Enlargement: 1

# unif_ellipsoid can provide good results

    c(n_iter = 4852, log_evidence = -6.68020446315307, log_evidence_var = 0.0150086922238039
    )

---

    c(n_iter = 5842, log_evidence = -8.95796791701933, log_evidence_var = 0.0191623941779136
    )

