# calculate works when ndraws = 0

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      Log. Volume: -17 ± NA
      Log. Evidence: -9.1 ± NA
      # A tibble: 10,456 x 5
            log_lik   log_volume log_weight log_evidence log_evidence_err
         <rvar[1d]>   <rvar[1d]> <rvar[1d]>   <rvar[1d]>       <rvar[1d]>
       1  -147 ± NA  -0.001 ± NA  -154 ± NA    -154 ± NA     7.3e-33 ± NA
       2  -141 ± NA  -0.002 ± NA  -148 ± NA    -148 ± NA     1.9e-31 ± NA
       3  -136 ± NA  -0.003 ± NA  -143 ± NA    -143 ± NA     1.9e-30 ± NA
       4  -136 ± NA  -0.004 ± NA  -142 ± NA    -142 ± NA     3.9e-30 ± NA
       5  -134 ± NA  -0.005 ± NA  -140 ± NA    -140 ± NA     8.6e-30 ± NA
       6  -130 ± NA  -0.006 ± NA  -137 ± NA    -137 ± NA     4.0e-29 ± NA
       7  -130 ± NA  -0.007 ± NA  -137 ± NA    -136 ± NA     7.1e-29 ± NA
       8  -129 ± NA  -0.008 ± NA  -136 ± NA    -135 ± NA     1.1e-28 ± NA
       9  -129 ± NA  -0.009 ± NA  -136 ± NA    -135 ± NA     1.6e-28 ± NA
      10  -126 ± NA  -0.010 ± NA  -132 ± NA    -132 ± NA     4.2e-28 ± NA
      # i 10,446 more rows

# calculate works when ndraws = 1

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      No. of Simulated Draws: 1
      Log. Volume: -20 ± NA
      Log. Evidence: -9.1 ± NA
      # A tibble: 10,456 x 4
            log_lik     log_volume log_weight log_evidence
         <rvar[1d]>     <rvar[1d]> <rvar[1d]>   <rvar[1d]>
       1  -147 ± NA  -0.00028 ± NA  -155 ± NA    -155 ± NA
       2  -141 ± NA  -0.00104 ± NA  -148 ± NA    -148 ± NA
       3  -136 ± NA  -0.00249 ± NA  -143 ± NA    -143 ± NA
       4  -136 ± NA  -0.00416 ± NA  -142 ± NA    -142 ± NA
       5  -134 ± NA  -0.00556 ± NA  -140 ± NA    -140 ± NA
       6  -130 ± NA  -0.00660 ± NA  -137 ± NA    -137 ± NA
       7  -130 ± NA  -0.00705 ± NA  -138 ± NA    -137 ± NA
       8  -129 ± NA  -0.00718 ± NA  -136 ± NA    -136 ± NA
       9  -129 ± NA  -0.00869 ± NA  -136 ± NA    -135 ± NA
      10  -126 ± NA  -0.00915 ± NA  -133 ± NA    -133 ± NA
      # i 10,446 more rows

# calculate works when ndraws = 1000 (default)

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      No. of Simulated Draws: 1000
      Log. Volume: -17 ± 1.2
      Log. Evidence: -9.1 ± 0.071
      # A tibble: 10,456 x 4
            log_lik          log_volume   log_weight log_evidence
         <rvar[1d]>          <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1  -147 ± NA  -0.00095 ± 0.00096  -154 ± 0.82  -154 ± 0.82
       2  -141 ± NA  -0.00197 ± 0.00141  -148 ± 0.80  -148 ± 0.79
       3  -136 ± NA  -0.00293 ± 0.00169  -143 ± 0.77  -143 ± 0.76
       4  -136 ± NA  -0.00393 ± 0.00200  -143 ± 0.80  -142 ± 0.65
       5  -134 ± NA  -0.00493 ± 0.00221  -141 ± 0.79  -140 ± 0.66
       6  -130 ± NA  -0.00599 ± 0.00246  -137 ± 0.81  -137 ± 0.77
       7  -130 ± NA  -0.00696 ± 0.00266  -137 ± 0.79  -136 ± 0.64
       8  -129 ± NA  -0.00796 ± 0.00279  -136 ± 0.82  -135 ± 0.59
       9  -129 ± NA  -0.00904 ± 0.00299  -136 ± 0.79  -135 ± 0.54
      10  -126 ± NA  -0.01004 ± 0.00314  -133 ± 0.79  -133 ± 0.67
      # i 10,446 more rows

