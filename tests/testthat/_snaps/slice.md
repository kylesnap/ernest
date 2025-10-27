# slice returns correct class and structure

    Code
      obj
    Output
      slice sampler LRPS <slice/ernest_lrps>
      
      No. Dimensions: Uninitialized

# new_slice: errors on invalid arguments

    Code
      new_slice(fn, 2L, enlarge = 0.5)
    Condition
      Error in `new_slice()`:
      ! `enlarge` must be a number larger than or equal to 1 or `NA`, not the number 0.5.

---

    Code
      slice(enlarge = 0)
    Condition
      Error in `slice()`:
      ! `enlarge` must be a number larger than or equal to 1 or `NA`, not the number 0.

---

    Code
      slice(enlarge = "invalid")
    Condition
      Error in `slice()`:
      ! `enlarge` must be a number or `NA`, not the string "invalid".

# propose.slice: proposes a single new point

    Code
      slc
    Output
      slice sampler LRPS <slice/ernest_lrps>
      
      No. Dimensions: 2
      Centre: 0.5000, 0.5000
      Log Volume: 0.0000

