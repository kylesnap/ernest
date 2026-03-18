# calculate works when ndraws = 0

    Code
      calc
    Output
      # <ernest_estimate>: 10456 niter.
      # Log-volumes:       Expected values
         log_lik log_volume log_weight log_evidence log_evidence_err
           <dbl>      <dbl>      <dbl>        <dbl>            <dbl>
       1   -147.     -0.001      -154.        -154.         7.27e-33
       2   -141.     -0.002      -148.        -148.         1.88e-31
       3   -136.     -0.003      -143.        -143.         1.92e-30
       4   -136.     -0.004      -142.        -142.         3.87e-30
       5   -134.     -0.005      -140.        -140.         8.60e-30
       6   -130.     -0.006      -137.        -137.         4.00e-29
       7   -130.     -0.007      -137.        -136.         7.09e-29
       8   -129.     -0.008      -136.        -135.         1.13e-28
       9   -129.     -0.009      -136.        -135.         1.61e-28
      10   -126.     -0.01       -132.        -132.         4.17e-28
      # i 10,446 more rows

# calculate works when ndraws = 1

    Code
      calc
    Output
      # <ernest_estimate>: 10456 niter.
      # Log-volumes:       Simulated (`ndraws` = 1)
         log_lik     log_volume log_weight log_evidence
           <dbl>     <rvar[1d]> <rvar[1d]>   <rvar[1d]>
       1   -147.  -0.00028 ± NA  -155 ± NA    -155 ± NA
       2   -141.  -0.00104 ± NA  -148 ± NA    -148 ± NA
       3   -136.  -0.00249 ± NA  -143 ± NA    -143 ± NA
       4   -136.  -0.00416 ± NA  -142 ± NA    -142 ± NA
       5   -134.  -0.00556 ± NA  -140 ± NA    -140 ± NA
       6   -130.  -0.00660 ± NA  -137 ± NA    -137 ± NA
       7   -130.  -0.00705 ± NA  -138 ± NA    -137 ± NA
       8   -129.  -0.00718 ± NA  -136 ± NA    -136 ± NA
       9   -129.  -0.00869 ± NA  -136 ± NA    -135 ± NA
      10   -126.  -0.00915 ± NA  -133 ± NA    -133 ± NA
      # i 10,446 more rows

# calculate works when ndraws = 1000 (default)

    Code
      calc
    Output
      # <ernest_estimate>: 10456 niter.
      # Log-volumes:       Simulated (`ndraws` = 1000)
         log_lik          log_volume   log_weight log_evidence
           <dbl>          <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1   -147.  -0.00095 ± 0.00096  -154 ± 0.82  -154 ± 0.82
       2   -141.  -0.00197 ± 0.00141  -148 ± 0.80  -148 ± 0.79
       3   -136.  -0.00293 ± 0.00169  -143 ± 0.77  -143 ± 0.76
       4   -136.  -0.00393 ± 0.00200  -143 ± 0.80  -142 ± 0.65
       5   -134.  -0.00493 ± 0.00221  -141 ± 0.79  -140 ± 0.66
       6   -130.  -0.00599 ± 0.00246  -137 ± 0.81  -137 ± 0.77
       7   -130.  -0.00696 ± 0.00266  -137 ± 0.79  -136 ± 0.64
       8   -129.  -0.00796 ± 0.00279  -136 ± 0.82  -135 ± 0.59
       9   -129.  -0.00904 ± 0.00299  -136 ± 0.79  -135 ± 0.54
      10   -126.  -0.01004 ± 0.00314  -133 ± 0.79  -133 ± 0.67
      # i 10,446 more rows

