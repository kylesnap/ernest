# calculate works when ndraws = 0

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      Log. Volume: -17 ± NA
      Log. Evidence: -9.2 ± NA
      # A tibble: 10,495 x 5
            log_lik   log_volume log_weight log_evidence log_evidence_err
         <rvar[1d]>   <rvar[1d]> <rvar[1d]>   <rvar[1d]>       <rvar[1d]>
       1  -145 ± NA  -0.001 ± NA  -152 ± NA    -152 ± NA     2.3e-32 ± NA
       2  -144 ± NA  -0.002 ± NA  -150 ± NA    -150 ± NA     6.2e-32 ± NA
       3  -137 ± NA  -0.003 ± NA  -144 ± NA    -144 ± NA     1.5e-30 ± NA
       4  -133 ± NA  -0.004 ± NA  -140 ± NA    -140 ± NA     1.2e-29 ± NA
       5  -133 ± NA  -0.005 ± NA  -140 ± NA    -139 ± NA     2.0e-29 ± NA
       6  -131 ± NA  -0.006 ± NA  -138 ± NA    -137 ± NA     3.7e-29 ± NA
       7  -128 ± NA  -0.007 ± NA  -135 ± NA    -135 ± NA     1.1e-28 ± NA
       8  -128 ± NA  -0.008 ± NA  -135 ± NA    -134 ± NA     1.9e-28 ± NA
       9  -127 ± NA  -0.009 ± NA  -134 ± NA    -133 ± NA     3.1e-28 ± NA
      10  -126 ± NA  -0.010 ± NA  -133 ± NA    -133 ± NA     4.5e-28 ± NA
      # i 10,485 more rows

# calculate works when ndraws = 1

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      No. of Simulated Draws: 1
      Log. Volume: -15 ± NA
      Log. Evidence: -9.1 ± NA
      # A tibble: 10,495 x 4
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
      # i 10,485 more rows

# calculate works when ndraws = 4000 (default)

    Code
      calc
    Output
      evidence estimates <ernest_estimate>
      
      No. of Simulated Draws: 4000
      Log. Volume: -17 ± 1.3
      Log. Evidence: -9.2 ± 0.072
      # A tibble: 10,495 x 4
            log_lik          log_volume   log_weight log_evidence
         <rvar[1d]>          <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1  -145 ± NA  -0.00099 ± 0.00097  -152 ± 0.84  -152 ± 0.84
       2  -144 ± NA  -0.00199 ± 0.00140  -151 ± 0.81  -150 ± 0.68
       3  -137 ± NA  -0.00301 ± 0.00171  -144 ± 0.81  -144 ± 0.81
       4  -133 ± NA  -0.00401 ± 0.00198  -140 ± 0.81  -140 ± 0.79
       5  -133 ± NA  -0.00502 ± 0.00223  -140 ± 0.81  -139 ± 0.65
       6  -131 ± NA  -0.00605 ± 0.00245  -138 ± 0.81  -138 ± 0.63
       7  -128 ± NA  -0.00705 ± 0.00265  -136 ± 0.79  -135 ± 0.70
       8  -128 ± NA  -0.00807 ± 0.00283  -135 ± 0.79  -134 ± 0.61
       9  -127 ± NA  -0.00911 ± 0.00301  -134 ± 0.80  -133 ± 0.58
      10  -126 ± NA  -0.01009 ± 0.00314  -134 ± 0.82  -133 ± 0.54
      # i 10,485 more rows

