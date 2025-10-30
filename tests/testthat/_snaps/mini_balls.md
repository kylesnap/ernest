# mini_balls can be called by user

    Code
      mini_balls(enlarge = 0.5)
    Condition
      Error in `mini_balls()`:
      ! `enlarge` must be a number larger than or equal to 1, not the number 0.5.

---

    Code
      mini_balls(enlarge = 1)
    Message
      ! `enlarge` is set to 1.0, which is not recommended.
      <mini_balls> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      Distance: euclidean
      Radius: Undefined
      Enlargement: 1
      

---

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
      <mini_balls> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      Distance: euclidean
      Radius: Undefined
      Enlargement: 1.1
      

# mini_balls works with non-euclidean norms:: manhattan

    Code
      obj
    Message
      <mini_balls> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      Distance: manhattan
      Radius: Undefined
      Enlargement: 1.1
      

# mini_balls works with non-euclidean norms:: maximum

    Code
      obj
    Message
      <mini_balls> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      Distance: maximum
      Radius: Undefined
      Enlargement: 1.1
      

# mini_balls works with non-euclidean norms:: 3-norm

    Code
      obj
    Message
      <mini_balls> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      Distance: 3-norm
      Radius: Undefined
      Enlargement: 1.1
      

# update throws a warning when the points are all identical

    Code
      update_lrps(obj, live)
    Condition
      Warning in `min()`:
      no non-missing arguments to min; returning Inf
      Warning:
      Distances between points are non-finite. The radius is set to `-Inf`.
    Message
      <mini_balls> lrps:
      No. Dimensions: 2
      No. Calls Since Update: 0
      Distance: euclidean
      Radius: Undefined
      Enlargement: 1
      

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
      

