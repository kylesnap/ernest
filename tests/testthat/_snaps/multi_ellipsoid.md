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
      <multi_ellipsoid> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      No. Ellipsoids: 0
      Total Log Volume: -Inf
      Min Reduction: 0.7
      Allow Contact: TRUE
      Enlargement: 1
      

---

    Code
      multi_ellipsoid(min_reduction = 1)
    Condition
      Warning:
      `min_reduction` is set to 1, which may lead to over-splitting.
      i Should `allow_contact` be set to `FALSE`?
    Message
      <multi_ellipsoid> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      No. Ellipsoids: 0
      Total Log Volume: -Inf
      Min Reduction: 1
      Allow Contact: TRUE
      Enlargement: 1.25
      

---

    Code
      multi_ellipsoid(min_reduction = -0.1)
    Condition
      Error in `multi_ellipsoid()`:
      ! `min_reduction` must be a number between 0 and 1, not the number -0.1.

---

    Code
      multi_ellipsoid(min_reduction = 1.1)
    Condition
      Error in `multi_ellipsoid()`:
      ! `min_reduction` must be a number between 0 and 1, not the number 1.1.

---

    Code
      multi_ellipsoid(allow_contact = "boop")
    Condition
      Error in `multi_ellipsoid()`:
      ! `allow_contact` must be `TRUE` or `FALSE`, not the string "boop".

---

    Code
      default
    Message
      <multi_ellipsoid> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      No. Ellipsoids: 0
      Total Log Volume: -Inf
      Min Reduction: 0.7
      Allow Contact: TRUE
      Enlargement: 1.25
      

# multi_ellipsoid class: Can call propose

    Code
      as.list(lrps$cache)
    Output
      $prob
      [1] 1
      
      $ellipsoid
      $ellipsoid[[1]]
      $ellipsoid[[1]]$center
      [1] 0.5 0.5
      
      $ellipsoid[[1]]$shape
           [,1] [,2]
      [1,]    2    0
      [2,]    0    2
      
      $ellipsoid[[1]]$inv_sqrt_shape
                [,1]      [,2]
      [1,] 0.7071068 0.0000000
      [2,] 0.0000000 0.7071068
      
      $ellipsoid[[1]]$log_vol
      [1] 0.4515827
      
      $ellipsoid[[1]]$error
      [1] 0
      
      
      
      $total_log_volume
      [1] 0.4515827
      
      $n_call
      [1] 8
      

# multi_ellipsoid can provide good results

    Code
      result
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "No. Ellipsoids: 1"        "Total Log Volume: 0.4516"
      [3] "Min Reduction: 0.7"       "Allow Contact: TRUE"     
      [5] "Enlargement: 1.25"       
      
      -- Results 
      No. Iterations: 4860
      No. Calls: 10231
      Log. Evidence: -6.697 (± 0.1232)

---

    Code
      result
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "No. Ellipsoids: 1"        "Total Log Volume: 0.4516"
      [3] "Min Reduction: 0.7"       "Allow Contact: TRUE"     
      [5] "Enlargement: 1.25"       
      
      -- Results 
      No. Iterations: 4878
      No. Calls: 9956
      Log. Evidence: -6.733 (± 0.1235)

---

    Code
      result
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "No. Ellipsoids: 1"       "Total Log Volume: 1.001"
      [3] "Min Reduction: 0.7"      "Allow Contact: TRUE"    
      [5] "Enlargement: 1.25"      
      
      -- Results 
      No. Iterations: 5775
      No. Calls: 12523
      Log. Evidence: -8.828 (± 0.1375)

---

    Code
      result
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "No. Ellipsoids: 1"        "Total Log Volume: 0.4516"
      [3] "Min Reduction: 0.5"       "Allow Contact: TRUE"     
      [5] "Enlargement: 1.25"       
      
      -- Results 
      No. Iterations: 4949
      No. Calls: 25872
      Log. Evidence: 236.1 (± 0.1182)

# update throws a warning when the points are all identical

    Code
      update_lrps(obj, live)
    Condition
      Warning:
      Multi-ellipsoid fitting returned an error code (1).
    Message
      <multi_ellipsoid> lrps:
      No. Dimensions: 2
      No. Calls Since Update: 0
      No. Ellipsoids: 1
      Total Log Volume: 0.4516
      Min Reduction: 0.7
      Allow Contact: FALSE
      Enlargement: 1
      

