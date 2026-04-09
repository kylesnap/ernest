# calculate / works when ndraws = 0

    Code
      calc
    Output
      # <ernest_estimate>:  10359 niter.
      # Uncertainty source: Normally-Approximated Analytical Estimates (1000 draws)
         log_lik  log_volume log_weight log_evidence
           <dbl>  <rvar[1d]> <rvar[1d]>   <rvar[1d]>
       1   -137.  -5.2 ± 3.1  -143 ± NA     -143 ± 0
       2   -132.  -5.2 ± 3.1  -139 ± NA     -139 ± 0
       3   -130.  -5.2 ± 3.1  -137 ± NA     -137 ± 0
       4   -130.  -5.2 ± 3.1  -137 ± NA     -136 ± 0
       5   -129.  -5.2 ± 3.1  -136 ± NA     -136 ± 0
       6   -127.  -5.2 ± 3.1  -134 ± NA     -134 ± 0
       7   -124.  -5.2 ± 3.1  -131 ± NA     -131 ± 0
       8   -123.  -5.2 ± 3.1  -130 ± NA     -130 ± 0
       9   -123.  -5.2 ± 3.1  -130 ± NA     -129 ± 0
      10   -122.  -5.2 ± 3.1  -129 ± NA     -129 ± 0
      # i 10,349 more rows

# calculate / works when ndraws = 1000 (default)

    Code
      calc
    Output
      # <ernest_estimate>:  10359 niter.
      # Uncertainty source: Simulated Log-Volumes (1000 draws)
         log_lik         log_volume   log_weight log_evidence
           <dbl>         <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1   -137.  -0.0010 ± 0.00098  -144 ± 0.78  -144 ± 0.78
       2   -132.  -0.0021 ± 0.00146  -139 ± 0.80  -139 ± 0.79
       3   -130.  -0.0032 ± 0.00184  -137 ± 0.80  -137 ± 0.69
       4   -130.  -0.0042 ± 0.00208  -137 ± 0.77  -136 ± 0.57
       5   -129.  -0.0051 ± 0.00222  -137 ± 0.77  -136 ± 0.49
       6   -127.  -0.0061 ± 0.00241  -134 ± 0.76  -134 ± 0.62
       7   -124.  -0.0071 ± 0.00262  -132 ± 0.78  -131 ± 0.71
       8   -123.  -0.0081 ± 0.00285  -130 ± 0.80  -130 ± 0.67
       9   -123.  -0.0091 ± 0.00295  -130 ± 0.80  -129 ± 0.58
      10   -122.  -0.0101 ± 0.00310  -130 ± 0.82  -129 ± 0.54
      # i 10,349 more rows

