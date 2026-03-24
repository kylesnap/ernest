# calculate works when ndraws = 0

    Code
      calc
    Output
      # <ernest_estimate>: 10348 niter.
      # Log-volumes:       Expected values
         log_lik log_volume log_weight log_evidence log_evidence_err
           <dbl>      <dbl>      <dbl>        <dbl>            <dbl>
       1   -137.     -0.001      -143.        -143.         1.30e-30
       2   -132.     -0.002      -139.        -139.         1.63e-29
       3   -130.     -0.003      -137.        -137.         4.20e-29
       4   -130.     -0.004      -137.        -136.         7.10e-29
       5   -129.     -0.005      -136.        -136.         1.01e-28
       6   -127.     -0.006      -134.        -134.         2.11e-28
       7   -124.     -0.007      -131.        -131.         6.96e-28
       8   -123.     -0.008      -130.        -130.         1.53e-27
       9   -123.     -0.009      -130.        -129.         2.46e-27
      10   -122.     -0.01       -129.        -129.         3.35e-27
      # i 10,338 more rows

# calculate works when ndraws = 1

    Code
      calc
    Output
      # <ernest_estimate>: 10348 niter.
      # Log-volumes:       Simulated (`ndraws` = 1)
         log_lik     log_volume log_weight log_evidence
           <dbl>     <rvar[1d]> <rvar[1d]>   <rvar[1d]>
       1   -137.  -0.00028 ± NA  -144 ± NA    -144 ± NA
       2   -132.  -0.00104 ± NA  -139 ± NA    -139 ± NA
       3   -130.  -0.00249 ± NA  -137 ± NA    -137 ± NA
       4   -130.  -0.00416 ± NA  -136 ± NA    -136 ± NA
       5   -129.  -0.00556 ± NA  -136 ± NA    -135 ± NA
       6   -127.  -0.00660 ± NA  -134 ± NA    -134 ± NA
       7   -124.  -0.00705 ± NA  -133 ± NA    -132 ± NA
       8   -123.  -0.00718 ± NA  -130 ± NA    -130 ± NA
       9   -123.  -0.00869 ± NA  -130 ± NA    -129 ± NA
      10   -122.  -0.00915 ± NA  -130 ± NA    -129 ± NA
      # i 10,338 more rows

# calculate works when ndraws = 1000 (default)

    Code
      calc
    Output
      # <ernest_estimate>: 10348 niter.
      # Log-volumes:       Simulated (`ndraws` = 1000)
         log_lik          log_volume   log_weight log_evidence
           <dbl>          <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1   -137.  -0.00094 ± 0.00095  -144 ± 0.84  -144 ± 0.84
       2   -132.  -0.00195 ± 0.00142  -139 ± 0.79  -139 ± 0.78
       3   -130.  -0.00293 ± 0.00172  -137 ± 0.77  -137 ± 0.67
       4   -130.  -0.00393 ± 0.00200  -137 ± 0.79  -136 ± 0.59
       5   -129.  -0.00493 ± 0.00222  -137 ± 0.79  -136 ± 0.54
       6   -127.  -0.00597 ± 0.00248  -134 ± 0.81  -134 ± 0.66
       7   -124.  -0.00696 ± 0.00265  -132 ± 0.78  -131 ± 0.71
       8   -123.  -0.00795 ± 0.00281  -130 ± 0.83  -130 ± 0.68
       9   -123.  -0.00902 ± 0.00300  -130 ± 0.79  -129 ± 0.60
      10   -122.  -0.01004 ± 0.00315  -130 ± 0.79  -129 ± 0.52
      # i 10,338 more rows

