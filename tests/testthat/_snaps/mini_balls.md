# mini_balls constructor: sets defaults and warns for enlarge = 1

    Code
      obj <- mini_balls(enlarge = 1)
    Message
      ! `enlarge` is set to 1.0, which is not recommended.

# update_lrps.mini_balls: warns if all points are identical

    Code
      update_lrps(sampler, live)
    Condition
      Warning in `min()`:
      no non-missing arguments to min; returning Inf
      Warning:
      Distances between points are non-finite. The radius will not be updated.
    Output
      uniform p-norm ball LRPS <mini_balls/ernest_lrps>
      
      No. Dimensions: 2

