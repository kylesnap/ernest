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
      ernest LRPS method <slice_rectangle/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Centre: Undefined
      * Enlargement: Disabled

---

    Code
      default
    Message
      ernest LRPS method <slice_rectangle/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Centre: Undefined
      * Enlargement: 1

