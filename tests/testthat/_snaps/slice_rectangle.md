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

# slice class: Can be constructed with new_

    Code
      obj
    Message
      ernest LRPS method <slice_rectangle/ernest_lrps>
      * Dimensions: 2
      * No. Log-Lik Calls: 0
      * Centre: 0.5000, 0.5000
      * Enlargement: 1.25

# slice class: Can call propose

    Code
      as.list(lrps$cache)
    Output
      $lower
      [1] 0 0
      
      $upper
      [1] 1 1
      
      $n_call
      [1] 2
      
      $n_accept
      [1] 0
      

# slice can provide good results

    Code
      result
    Message
      nested sampling results <ernest_run/ernest_sampler>
      * No. Points: 500
      * LRPS Method: slice_rectangle
      --------------------------------------------------------------------------------
      * No. Iterations: 4862
      * No. Calls: 14209
      * Log. Evidence: -6.701 (± 0.1230)

---

    Code
      result
    Message
      nested sampling results <ernest_run/ernest_sampler>
      * No. Points: 500
      * LRPS Method: slice_rectangle
      --------------------------------------------------------------------------------
      * No. Iterations: 5811
      * No. Calls: 21803
      * Log. Evidence: -8.897 (± 0.1375)

---

    Code
      result
    Message
      nested sampling results <ernest_run/ernest_sampler>
      * No. Points: 500
      * LRPS Method: slice_rectangle
      --------------------------------------------------------------------------------
      * No. Iterations: 4992
      * No. Calls: 32286
      * Log. Evidence: 236.0 (± 0.1186)

