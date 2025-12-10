# mini_balls can be called by user

    Code
      mini_balls(method = "swoop")
    Condition
      Error in `new_mini_balls()`:
      ! `method` must be one of "euclidean" or "maximum", not "swoop".

---

    Code
      mini_balls(bootstrap = -1L)
    Condition
      Error in `new_mini_balls()`:
      ! `bootstrap` must be a whole number larger than or equal to 1 or `NULL`, not the number -1.

---

    Code
      default
    Message
      ernest LRPS method <mini_balls/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Method: euclidean
      * Bootstrap: Disabled
      * Radius: Undefined

# mini_balls works with non-defaults:: bootstrapped euclidean

    Code
      obj
    Message
      ernest LRPS method <mini_balls/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Method: euclidean
      * Bootstrap: 30
      * Radius: Undefined

# update throws a warning when the points are all identical

    Code
      update_lrps(obj, live)
    Condition
      Warning:
      `radius` must be a finite, positive value, not 0.
      ! Falling-back to uniform hypercube sampling.
    Message
      ernest LRPS method <mini_balls/ernest_lrps>
      * Dimensions: 2
      * No. Log-Lik Calls: 0
      * Method: euclidean
      * Bootstrap: Disabled
      * Radius: Undefined

---

    Code
      propose(obj, c(0.5, 0.5), -Inf)
    Output
      $unit
      [1] 0.8975114 0.5099074
      
      $log_lik
      [1] -483.1485
      
      $n_call
      [1] 1
      

