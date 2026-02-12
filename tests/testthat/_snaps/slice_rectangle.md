# slice can be called by user

    Code
      slice_rectangle(steps = 0)
    Condition
      Error in `slice_rectangle()`:
      ! `steps` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      slice_rectangle(steps = NA)
    Condition
      Error in `slice_rectangle()`:
      ! `steps` must be a whole number, not `NA`.

---

    Code
      default
    Message
      Slice Sampling LRPS (1 Step):
      # Dimensions: Uninitialized
      # Calls since last update: 0
      

# slice class / Can be built and propose points

    Code
      obj
    Message
      Slice Sampling LRPS (1 Step):
      # Dimensions: 2
      # Calls since last update: 65
      

