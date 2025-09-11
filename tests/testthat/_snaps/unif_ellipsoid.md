# unif_ellipsoid returns correct class and structure

    Code
      obj
    Output
      Uniform Ellipsoid LRPS <unif_ellipsoid/ernest_lrps>
      No. Dimensions: Uninitialized
      No. Calls Since Update: 0

# new_unif_ellipsoid: fails when scale or n_dim are improper.

    Code
      new_unif_ellipsoid(fn, 2L, scale = 0.5)
    Condition
      Error in `new_unif_ellipsoid()`:
      ! `scale` must be a number larger than or equal to 1, not the number 0.5.

---

    Code
      new_unif_ellipsoid(fn, 1L)
    Condition
      Error in `new_unif_ellipsoid()`:
      ! `n_dim` must be larger than 1.

# new_unif_ellipsoid: passes the correct defaults

    Code
      obj
    Output
      Uniform Ellipsoid LRPS <unif_ellipsoid/ernest_lrps>
      No. Dimensions: 2
      No. Calls Since Update: 0
      Centre: 0.5 and 0.5
      Volume: 1.5708

# propose.unif_ellipsoid: Proposes points in the unit sphere

    Code
      uniform
    Output
      Uniform Ellipsoid LRPS <unif_ellipsoid/ernest_lrps>
      No. Dimensions: 2
      No. Calls Since Update: 8
      Centre: 0.5 and 0.5
      Volume: 1.5708

# update_lrps.unif_ellipsoid: can rebound to a matrix of live points

    Code
      uniform
    Output
      Uniform Ellipsoid LRPS <unif_ellipsoid/ernest_lrps>
      No. Dimensions: 2
      No. Calls Since Update: 0
      Centre: 0.5003 and 0.5052
      Volume: 0.1508

# update_lrps.unif_ellipsoid: fails to a unit sphere when cluster fails

    Code
      new_uniform <- update_lrps(uniform, xy)
    Output
      Error in Fortran routine computing the spanning ellipsoid,
       probably collinear data
    Condition
      Warning:
      Sampling from the unit sphere after encountering a rebounding error.
      Caused by error in `update_lrps()`:
      ! Failed to estimate the spanning ellipsoid.
      x Error thrown by the Fortran routine.

