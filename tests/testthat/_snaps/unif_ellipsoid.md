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
      <unif_ellipsoid> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      Center: Undefined
      Log Volume: -Inf
      Enlargement: 1
      

---

    Code
      default
    Message
      <unif_ellipsoid> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      Center: Undefined
      Log Volume: -Inf
      Enlargement: 1.25
      

# update throws a warning when the points are all identical

    Code
      update_lrps(obj, live)
    Condition
      Warning:
      Ellipsoid fitting returned an error code (1).
    Message
      <unif_ellipsoid> lrps:
      No. Dimensions: 2
      No. Calls Since Update: 0
      Center: 0.5000, 0.5000
      Log Volume: 0.4516
      Enlargement: 1
      

