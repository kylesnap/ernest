# mini_balls can be called by user

    Code
      mini_balls(method = "swoop")
    Condition
      Error in `mini_balls()`:
      ! `method` must be one of "euclidean", "maximum", or "manhattan", not "swoop".

---

    Code
      mini_balls(p = 0)
    Condition
      Error in `mini_balls()`:
      ! `p` must be a number larger than or equal to 1, not the number 0.

---

    Code
      default
    Message
      ernest LRPS method <mini_balls/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Distance: Euclidean
      * Radius: Undefined

# mini_balls works with non-euclidean norms:: manhattan

    Code
      obj
    Message
      ernest LRPS method <mini_balls/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Distance: Manhattan
      * Radius: Undefined

# mini_balls works with non-euclidean norms:: maximum

    Code
      obj
    Message
      ernest LRPS method <mini_balls/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Distance: Maximum
      * Radius: Undefined

# mini_balls works with non-euclidean norms:: 3-norm

    Code
      obj
    Message
      ernest LRPS method <mini_balls/ernest_lrps>
      * Dimensions: 'Undefined'
      * No. Log-Lik Calls: 0
      * Distance: 3-norm
      * Radius: Undefined

# update throws a warning when the points are all identical

    Code
      update_lrps(obj, live)
    Condition
      Warning:
      Encountered an error rebounding the sampler.
      ! Setting `radius` to -Inf.
      Caused by error in `update_lrps()`:
      ! The max. min. distance between live points can't be zero.
    Message
      ernest LRPS method <mini_balls/ernest_lrps>
      * Dimensions: 2
      * No. Log-Lik Calls: 0
      * Distance: Euclidean
      * Radius: Undefined

---

    Code
      propose(obj, c(0.5, 0.5), -Inf)
    Condition
      Warning:
      `x` does not have a valid radius to sample within.
    Output
      $unit
      [1] 0.9040314 0.1387102
      
      $log_lik
      [1] -1526.117
      
      $n_call
      [1] 1
      

# mini_balls can provide good results

    c(n_iter = 2420, log_evidence = -6.65236373847275, log_evidence_var = 0.0293383120912959
    )

