# new_unif_ellipsoid: fails when scale or n_dim are improper.

    Code
      new_unif_ellipsoid(fn, 2L, enlarge = 0.5)
    Condition
      Error in `new_unif_ellipsoid()`:
      ! `enlarge` must be a number larger than or equal to 1, not the number 0.5.

# new_unif_ellipsoid: passes the correct defaults

    Code
      obj
    Output
      uniform ellipsoid LRPS <unif_ellipsoid/ernest_lrps>
      
      No. Dimensions: 2
      Centre: 0.5000, 0.5000
      Log Volume: 0.4516

# propose.unif_ellipsoid: Proposes points in the unit sphere

    Code
      uniform
    Output
      uniform ellipsoid LRPS <unif_ellipsoid/ernest_lrps>
      
      No. Dimensions: 2
      Centre: 0.5000, 0.5000
      Log Volume: 0.4516
      Enlargement Factor: 1.2

# update_lrps.unif_ellipsoid: can rebound to a matrix of live points

    Code
      uniform
    Output
      uniform ellipsoid LRPS <unif_ellipsoid/ernest_lrps>
      
      No. Dimensions: 2
      Centre: 0.5010, 0.4957
      Log Volume: -1.529
      Enlargement Factor: 1.2

# update_lrps.unif_ellipsoid: reports numerical errors

    Code
      new_uniform <- update_lrps(uniform, xy)
    Condition
      Warning:
      Ellipsoid fitting returned an error code (1).

