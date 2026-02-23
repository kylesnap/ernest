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
      Slice Sampling LRPS :
      # Dimensions: Uninitialized
      # Calls since last update: 0
      

---

    Code
      default
    Message
      Slice Sampling LRPS (enlarged by 1):
      # Dimensions: Uninitialized
      # Calls since last update: 0
      

# slice class / Can be built and propose points

    Code
      obj
    Message
      Slice Sampling LRPS (enlarged by 1.25):
      # Dimensions: 2
      # Calls since last update: 65
      

