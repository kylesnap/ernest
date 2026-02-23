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
      25-step random walk sampling (acceptance target = 50.0%):
      # Dimensions: Uninitialized
      # Calls since last update: 0
      

