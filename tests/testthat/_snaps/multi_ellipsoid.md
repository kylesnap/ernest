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
      Uniform sampling within bounding ellipsoids (enlarged by 1):
      # Dimensions: Uninitialized
      # Calls since last update: 0
      

---

    Code
      default
    Message
      Uniform sampling within bounding ellipsoids (enlarged by 1.25):
      # Dimensions: Uninitialized
      # Calls since last update: 0
      

