# calculate works when ndraws = 0

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      Log. Volume: -17 ± NA
      Log. Evidence: -9.1 ± NA
      # A tibble: 10,473 x 5
            log_lik   log_volume log_weight log_evidence log_evidence_err
         <rvar[1d]>   <rvar[1d]> <rvar[1d]>   <rvar[1d]>       <rvar[1d]>
       1  -145 ± NA  -0.001 ± NA  -152 ± NA    -152 ± NA     2.3e-32 ± NA
       2  -144 ± NA  -0.002 ± NA  -150 ± NA    -150 ± NA     6.1e-32 ± NA
       3  -137 ± NA  -0.003 ± NA  -144 ± NA    -144 ± NA     1.5e-30 ± NA
       4  -133 ± NA  -0.004 ± NA  -140 ± NA    -140 ± NA     1.1e-29 ± NA
       5  -133 ± NA  -0.005 ± NA  -140 ± NA    -139 ± NA     2.0e-29 ± NA
       6  -131 ± NA  -0.006 ± NA  -138 ± NA    -137 ± NA     3.7e-29 ± NA
       7  -128 ± NA  -0.007 ± NA  -135 ± NA    -135 ± NA     1.1e-28 ± NA
       8  -128 ± NA  -0.008 ± NA  -135 ± NA    -134 ± NA     1.9e-28 ± NA
       9  -127 ± NA  -0.009 ± NA  -134 ± NA    -133 ± NA     3.0e-28 ± NA
      10  -126 ± NA  -0.010 ± NA  -133 ± NA    -133 ± NA     4.4e-28 ± NA
      # i 10,463 more rows

# calculate works when ndraws = 1

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      No. of Simulated Draws: 1
      Log. Volume: -17 ± NA
      Log. Evidence: -9.1 ± NA
      # A tibble: 10,473 x 4
            log_lik     log_volume log_weight log_evidence
         <rvar[1d]>     <rvar[1d]> <rvar[1d]>   <rvar[1d]>
       1  -145 ± NA  -0.00028 ± NA  -152 ± NA    -152 ± NA
       2  -144 ± NA  -0.00104 ± NA  -150 ± NA    -150 ± NA
       3  -137 ± NA  -0.00249 ± NA  -143 ± NA    -143 ± NA
       4  -133 ± NA  -0.00416 ± NA  -139 ± NA    -139 ± NA
       5  -133 ± NA  -0.00556 ± NA  -139 ± NA    -139 ± NA
       6  -131 ± NA  -0.00660 ± NA  -138 ± NA    -138 ± NA
       7  -128 ± NA  -0.00705 ± NA  -137 ± NA    -136 ± NA
       8  -128 ± NA  -0.00718 ± NA  -135 ± NA    -135 ± NA
       9  -127 ± NA  -0.00869 ± NA  -134 ± NA    -133 ± NA
      10  -126 ± NA  -0.00915 ± NA  -134 ± NA    -133 ± NA
      # i 10,463 more rows

