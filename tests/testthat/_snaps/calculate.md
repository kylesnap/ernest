# calculate / works when ndraws = 0

    Code
      calc
    Output
      # <ernest_estimate>:  10359 niter.
      # Uncertainty source: Normally-Approximated Analytical Estimates (1000 draws)
         log_lik   log_volume log_weight log_evidence
           <dbl>   <rvar[1d]> <rvar[1d]>   <rvar[1d]>
       1   -137.  -0.001 ± NA  -143 ± NA     -143 ± 0
       2   -132.  -0.002 ± NA  -139 ± NA     -139 ± 0
       3   -130.  -0.003 ± NA  -137 ± NA     -137 ± 0
       4   -130.  -0.004 ± NA  -137 ± NA     -136 ± 0
       5   -129.  -0.005 ± NA  -136 ± NA     -136 ± 0
       6   -127.  -0.006 ± NA  -134 ± NA     -134 ± 0
       7   -124.  -0.007 ± NA  -131 ± NA     -131 ± 0
       8   -123.  -0.008 ± NA  -130 ± NA     -130 ± 0
       9   -123.  -0.009 ± NA  -130 ± NA     -129 ± 0
      10   -122.  -0.010 ± NA  -129 ± NA     -129 ± 0
      # i 10,349 more rows

# calculate / works when ndraws = 1 (default)

    Code
      calc
    Output
      # <ernest_estimate>:  10359 niter.
      # Uncertainty source: Simulated Log-Volumes (1 draws)
         log_lik    log_volume log_weight log_evidence
           <dbl>    <rvar[1d]> <rvar[1d]>   <rvar[1d]>
       1   -137.  -0.0061 ± NA  -142 ± NA    -142 ± NA
       2   -132.  -0.0088 ± NA  -138 ± NA    -138 ± NA
       3   -130.  -0.0116 ± NA  -137 ± NA    -136 ± NA
       4   -130.  -0.0121 ± NA  -137 ± NA    -136 ± NA
       5   -129.  -0.0134 ± NA  -137 ± NA    -136 ± NA
       6   -127.  -0.0134 ± NA  -134 ± NA    -134 ± NA
       7   -124.  -0.0148 ± NA  -132 ± NA    -132 ± NA
       8   -123.  -0.0149 ± NA  -131 ± NA    -131 ± NA
       9   -123.  -0.0156 ± NA  -131 ± NA    -130 ± NA
      10   -122.  -0.0158 ± NA  -131 ± NA    -130 ± NA
      # i 10,349 more rows

# calculate / works when ndraws = 1000 (default)

    Code
      calc
    Output
      # <ernest_estimate>:  10359 niter.
      # Uncertainty source: Simulated Log-Volumes (1000 draws)
         log_lik        log_volume   log_weight log_evidence
           <dbl>        <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1   -137.  -0.0010 ± 0.0010  -144 ± 0.82  -144 ± 0.82
       2   -132.  -0.0020 ± 0.0014  -139 ± 0.79  -139 ± 0.77
       3   -130.  -0.0031 ± 0.0017  -137 ± 0.77  -137 ± 0.66
       4   -130.  -0.0041 ± 0.0020  -137 ± 0.79  -136 ± 0.59
       5   -129.  -0.0051 ± 0.0022  -137 ± 0.77  -136 ± 0.52
       6   -127.  -0.0061 ± 0.0024  -134 ± 0.78  -134 ± 0.63
       7   -124.  -0.0071 ± 0.0026  -132 ± 0.79  -131 ± 0.72
       8   -123.  -0.0081 ± 0.0027  -130 ± 0.79  -130 ± 0.65
       9   -123.  -0.0091 ± 0.0029  -130 ± 0.81  -129 ± 0.58
      10   -122.  -0.0100 ± 0.0031  -130 ± 0.79  -129 ± 0.53
      # i 10,349 more rows

