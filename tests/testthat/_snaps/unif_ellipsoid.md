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
      

# unif_ellipsoid class: Can call propose

    Code
      as.list(lrps$cache)
    Output
      $inv_sqrt_shape
                [,1]      [,2]
      [1,] 0.7071068 0.0000000
      [2,] 0.0000000 0.7071068
      
      $log_volume
      [1] 0.4515827
      
      $n_call
      [1] 7
      
      $shape
           [,1] [,2]
      [1,]    2    0
      [2,]    0    2
      
      $center
      [1] 0.5 0.5
      

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
      

# unif_ellipsoid can provide good results

    Code
      result
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "Center: 0.5000, 0.5000" "Log Volume: 0.4516"     "Enlargement: 1.25"     
      
      -- Results 
      No. Iterations: 4853
      No. Calls: 76003
      Log. Evidence: -6.682 (± 0.1231)

---

    Code
      result
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "Center: 0.5000, 0.5000" "Log Volume: 0.4516"     "Enlargement: 1.25"     
      
      -- Results 
      No. Iterations: 4802
      No. Calls: 73846
      Log. Evidence: -6.580 (± 0.1219)

---

    Code
      result
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "Center: 0.5000, 0.5000, 0.5000" "Log Volume: 1.001"             
      [3] "Enlargement: 1.25"             
      
      -- Results 
      No. Iterations: 5772
      No. Calls: 12718
      Log. Evidence: -8.819 (± 0.1371)

