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

