# calculate works when ndraws = 0

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      Log. Volume: -17 ± NA
      Log. Evidence: -9 ± NA
      # A tibble: 10,384 x 5
            log_lik   log_volume log_weight log_evidence log_evidence_err
         <rvar[1d]>   <rvar[1d]> <rvar[1d]>   <rvar[1d]>       <rvar[1d]>
       1  -147 ± NA  -0.001 ± NA  -154 ± NA    -154 ± NA     7.0e-33 ± NA
       2  -141 ± NA  -0.002 ± NA  -148 ± NA    -148 ± NA     1.8e-31 ± NA
       3  -136 ± NA  -0.003 ± NA  -143 ± NA    -143 ± NA     1.9e-30 ± NA
       4  -136 ± NA  -0.004 ± NA  -142 ± NA    -142 ± NA     3.7e-30 ± NA
       5  -134 ± NA  -0.005 ± NA  -140 ± NA    -140 ± NA     8.3e-30 ± NA
       6  -130 ± NA  -0.006 ± NA  -137 ± NA    -137 ± NA     3.9e-29 ± NA
       7  -130 ± NA  -0.007 ± NA  -137 ± NA    -136 ± NA     6.8e-29 ± NA
       8  -129 ± NA  -0.008 ± NA  -136 ± NA    -135 ± NA     1.1e-28 ± NA
       9  -129 ± NA  -0.009 ± NA  -136 ± NA    -135 ± NA     1.5e-28 ± NA
      10  -126 ± NA  -0.010 ± NA  -132 ± NA    -132 ± NA     4.0e-28 ± NA
      # i 10,374 more rows

# calculate works when ndraws = 1

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      No. of Simulated Draws: 1
      Log. Volume: -16 ± NA
      Log. Evidence: -9 ± NA
      # A tibble: 10,384 x 4
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
      # i 10,374 more rows

# calculate works when ndraws = 1000 (default)

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      No. of Simulated Draws: 1000
      Log. Volume: -17 ± 1.3
      Log. Evidence: -9 ± 0.071
      # A tibble: 10,384 x 4
            log_lik          log_volume   log_weight log_evidence
         <rvar[1d]>          <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1  -147 ± NA  -0.00095 ± 0.00095  -154 ± 0.83  -154 ± 0.83
       2  -141 ± NA  -0.00197 ± 0.00142  -148 ± 0.80  -148 ± 0.79
       3  -136 ± NA  -0.00293 ± 0.00170  -143 ± 0.77  -143 ± 0.76
       4  -136 ± NA  -0.00394 ± 0.00199  -143 ± 0.79  -142 ± 0.65
       5  -134 ± NA  -0.00493 ± 0.00221  -141 ± 0.79  -140 ± 0.66
       6  -130 ± NA  -0.00598 ± 0.00246  -137 ± 0.81  -137 ± 0.77
       7  -130 ± NA  -0.00696 ± 0.00265  -137 ± 0.78  -136 ± 0.64
       8  -129 ± NA  -0.00796 ± 0.00281  -136 ± 0.83  -135 ± 0.60
       9  -129 ± NA  -0.00903 ± 0.00300  -136 ± 0.79  -135 ± 0.54
      10  -126 ± NA  -0.01004 ± 0.00314  -133 ± 0.79  -133 ± 0.67
      # i 10,374 more rows

