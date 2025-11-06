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
      * Min Reduction: 0.7
      * Allow Contact: TRUE
      * Enlargement: 1

---

    Code
      multi_ellipsoid(min_reduction = 1)
    Condition
      Warning:
      `min_reduction` is set to 1, which may lead to over-splitting.
      i Should `allow_contact` be set to `FALSE`?
    Message
      ernest LRPS method <multi_ellipsoid/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * No. Ellipsoids: 0
      * Total Log Volume: -Inf
      * Min Reduction: 1
      * Allow Contact: TRUE
      * Enlargement: 1.25

---

    Code
      multi_ellipsoid(min_reduction = -0.1)
    Condition
      Error in `multi_ellipsoid()`:
      ! `min_reduction` must be a number between 0 and 1, not the number -0.1.

---

    Code
      multi_ellipsoid(min_reduction = 1.1)
    Condition
      Error in `multi_ellipsoid()`:
      ! `min_reduction` must be a number between 0 and 1, not the number 1.1.

---

    Code
      multi_ellipsoid(allow_contact = "boop")
    Condition
      Error in `multi_ellipsoid()`:
      ! `allow_contact` must be `TRUE` or `FALSE`, not the string "boop".

---

    Code
      default
    Message
      ernest LRPS method <multi_ellipsoid/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * No. Ellipsoids: 0
      * Total Log Volume: -Inf
      * Min Reduction: 0.7
      * Allow Contact: TRUE
      * Enlargement: 1.25

# multi_ellipsoid can provide good results

    c(n_iter = 4860, log_evidence = -6.69660957773848, log_evidence_var = 0.0151723046798601
    )

---

    c(n_iter = 5775, log_evidence = -8.82770314962741, log_evidence_var = 0.0189156102684251
    )

---

    c(n_iter = 4949, log_evidence = 236.125327999765, log_evidence_var = 0.0139697703521331
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
      * Min Reduction: 0.7
      * Allow Contact: FALSE
      * Enlargement: 1

