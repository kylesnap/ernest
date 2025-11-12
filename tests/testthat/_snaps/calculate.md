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

# calculate works when ndraws = 4000 (default)

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      No. of Simulated Draws: 4000
      Log. Volume: -17 ± 1.3
      Log. Evidence: -9 ± 0.072
      # A tibble: 10,384 x 4
            log_lik          log_volume   log_weight log_evidence
         <rvar[1d]>          <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1  -147 ± NA  -0.00099 ± 0.00097  -154 ± 0.84  -154 ± 0.84
       2  -141 ± NA  -0.00199 ± 0.00140  -148 ± 0.81  -148 ± 0.81
       3  -136 ± NA  -0.00301 ± 0.00171  -143 ± 0.81  -143 ± 0.80
       4  -136 ± NA  -0.00400 ± 0.00197  -143 ± 0.81  -142 ± 0.66
       5  -134 ± NA  -0.00502 ± 0.00223  -141 ± 0.81  -140 ± 0.67
       6  -130 ± NA  -0.00605 ± 0.00245  -137 ± 0.81  -137 ± 0.77
       7  -130 ± NA  -0.00705 ± 0.00264  -137 ± 0.79  -136 ± 0.63
       8  -129 ± NA  -0.00806 ± 0.00283  -136 ± 0.79  -135 ± 0.58
       9  -129 ± NA  -0.00911 ± 0.00301  -136 ± 0.80  -135 ± 0.53
      10  -126 ± NA  -0.01010 ± 0.00314  -133 ± 0.82  -133 ± 0.70
      # i 10,374 more rows

