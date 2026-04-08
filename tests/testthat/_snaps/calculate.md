# calculate works when ndraws = 0

    Code
      calc
    Output
      # <ernest_estimate>: 10359 niter.
      # Log-evidence:      -9.023975 (Expected log-vol.)
         log_lik log_volume log_weight log_evidence log_evidence_err
           <dbl>      <dbl>      <dbl>        <dbl>            <dbl>
       1   -137.     -0.001      -143.        -143.         1.31e-30
       2   -132.     -0.002      -139.        -139.         1.64e-29
       3   -130.     -0.003      -137.        -137.         4.23e-29
       4   -130.     -0.004      -137.        -136.         7.15e-29
       5   -129.     -0.005      -136.        -136.         1.02e-28
       6   -127.     -0.006      -134.        -134.         2.13e-28
       7   -124.     -0.007      -131.        -131.         7.01e-28
       8   -123.     -0.008      -130.        -130.         1.54e-27
       9   -123.     -0.009      -130.        -129.         2.47e-27
      10   -122.     -0.01       -129.        -129.         3.37e-27
      # i 10,349 more rows

# calculate works when ndraws = 1

    Code
      calc
    Output
      # <ernest_estimate>: 10359 niter.
      # Log-evidence:      -9 ± NA (Simulated log-vol., 1 draws)
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
      # i 10,349 more rows

# calculate works when ndraws = 1000 (default)

    Code
      calc
    Output
      # <ernest_estimate>: 10359 niter.
      # Log-evidence:      -9 ± 0.07 (Simulated log-vol., 1000 draws)
         log_lik          log_volume   log_weight log_evidence
           <dbl>          <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1   -137.  -0.00095 ± 0.00095  -144 ± 0.84  -144 ± 0.84
       2   -132.  -0.00196 ± 0.00141  -139 ± 0.79  -139 ± 0.78
       3   -130.  -0.00293 ± 0.00171  -137 ± 0.77  -137 ± 0.67
       4   -130.  -0.00394 ± 0.00199  -137 ± 0.79  -136 ± 0.59
       5   -129.  -0.00492 ± 0.00221  -137 ± 0.79  -136 ± 0.54
       6   -127.  -0.00597 ± 0.00247  -134 ± 0.81  -134 ± 0.66
       7   -124.  -0.00695 ± 0.00265  -132 ± 0.78  -131 ± 0.71
       8   -123.  -0.00796 ± 0.00281  -130 ± 0.83  -130 ± 0.69
       9   -123.  -0.00902 ± 0.00300  -130 ± 0.79  -129 ± 0.60
      10   -122.  -0.01004 ± 0.00315  -130 ± 0.79  -129 ± 0.52
      # i 10,349 more rows

