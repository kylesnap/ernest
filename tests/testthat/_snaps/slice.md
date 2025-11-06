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
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "Centre: 0.5000, 0.5000" "Log Volume: 0"          "Enlargement: 1"        
      
      -- Results 
      No. Iterations: 4862
      No. Calls: 14209
      Log. Evidence: -6.701 (± 0.1230)

---

    Code
      result
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "Centre: 0.5000, 0.5000, 0.5000" "Log Volume: 0"                 
      [3] "Enlargement: 1"                
      
      -- Results 
      No. Iterations: 5811
      No. Calls: 21803
      Log. Evidence: -8.897 (± 0.1375)

---

    Code
      result
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "Centre: 0.5000, 0.5000" "Log Volume: 0"          "Enlargement: 1"        
      
      -- Results 
      No. Iterations: 4992
      No. Calls: 32286
      Log. Evidence: 236.0 (± 0.1186)

