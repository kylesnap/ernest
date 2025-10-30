# slice can be called by user

    Code
      slice(enlarge = 0.5)
    Condition
      Error in `slice()`:
      ! `enlarge` must be a number larger than or equal to 1 or `NA`, not the number 0.5.

---

    Code
      slice(enlarge = NA)
    Message
      <slice> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      Centre: Undefined
      Log Volume: -Inf
      Enlargement: Disabled
      

---

    Code
      default
    Message
      <slice> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      Centre: Undefined
      Log Volume: -Inf
      Enlargement: 1
      

# slice class: Can be constructed with new_

    Code
      obj
    Message
      <slice> lrps:
      No. Dimensions: 2
      No. Calls Since Update: 0
      Centre: 0.5000, 0.5000
      Log Volume: 0
      Enlargement: 1.25
      

