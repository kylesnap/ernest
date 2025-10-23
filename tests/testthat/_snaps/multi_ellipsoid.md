# new_multi_ellipsoid: fails when parameters are improper

    Code
      new_multi_ellipsoid(fn, 2L, enlarge = 0.5)
    Condition
      Error in `new_multi_ellipsoid()`:
      ! `enlarge` must be a number larger than or equal to 1, not the number 0.5.

---

    Code
      new_multi_ellipsoid(fn, 2L, min_reduction = 1.5)
    Condition
      Error in `new_multi_ellipsoid()`:
      ! `min_reduction` must be a number between 0 and 1, not the number 1.5.

---

    Code
      new_multi_ellipsoid(fn, 2L, min_reduction = -0.1)
    Condition
      Error in `new_multi_ellipsoid()`:
      ! `min_reduction` must be a number between 0 and 1, not the number -0.1.

# new_multi_ellipsoid: passes the correct defaults

    Code
      obj
    Output
      multi ellipsoid LRPS <multi_ellipsoid/ernest_lrps>
      
      No. Dimensions: 2
      No. Ellipsoids: 1
      Total Log Volume: 0.4516
      Min Reduction: 0.7
      Allow Contact: TRUE

# update_lrps.multi_ellipsoid: can rebound to a matrix of live points

    Code
      new_multi
    Output
      multi ellipsoid LRPS <multi_ellipsoid/ernest_lrps>
      
      No. Dimensions: 2
      No. Ellipsoids: 2
      Total Log Volume: -2.064
      Min Reduction: 0.9
      Enlargement Factor: 1.2
      Allow Contact: TRUE

# update_lrps.multi_ellipsoid: reports numerical errors

    Code
      new_multi <- update_lrps(multi, xy)
    Condition
      Warning:
      Multi-ellipsoid fitting returned an error code (2).

