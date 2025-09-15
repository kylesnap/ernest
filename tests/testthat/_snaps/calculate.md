# calculate works when ndraws = 0

    Code
      calc
    Output
      Nested sampling estimates <ernest_estimate>
      Log. Volume: -16 ± NA
      Log. Evidence: -9.2 ± NA
      # A tibble: 5,260 x 5
            log_lik   log_volume log_weight log_evidence log_evidence_err
         <rvar[1d]>   <rvar[1d]> <rvar[1d]>   <rvar[1d]>       <rvar[1d]>
       1  -145 ± NA  -0.002 ± NA  -151 ± NA    -151 ± NA     4.6e-32 ± NA
       2  -144 ± NA  -0.004 ± NA  -150 ± NA    -149 ± NA     1.3e-31 ± NA
       3  -137 ± NA  -0.006 ± NA  -143 ± NA    -143 ± NA     3.0e-30 ± NA
       4  -128 ± NA  -0.008 ± NA  -135 ± NA    -135 ± NA     1.9e-28 ± NA
       5  -127 ± NA  -0.010 ± NA  -133 ± NA    -133 ± NA     5.0e-28 ± NA
       6  -125 ± NA  -0.012 ± NA  -132 ± NA    -131 ± NA     1.1e-27 ± NA
       7  -122 ± NA  -0.014 ± NA  -129 ± NA    -129 ± NA     3.9e-27 ± NA
       8  -122 ± NA  -0.016 ± NA  -128 ± NA    -128 ± NA     7.6e-27 ± NA
       9  -121 ± NA  -0.018 ± NA  -127 ± NA    -127 ± NA     1.2e-26 ± NA
      10  -119 ± NA  -0.020 ± NA  -126 ± NA    -125 ± NA     2.2e-26 ± NA
      # i 5,250 more rows

# calculate works when ndraws = 1

    Code
      calc
    Output
      Nested sampling estimates <ernest_estimate>
      No. of Simulated Draws: 1
      Log. Volume: -17 ± NA
      Log. Evidence: -9.3 ± NA
      # A tibble: 5,260 x 4
            log_lik     log_volume log_weight log_evidence
         <rvar[1d]>     <rvar[1d]> <rvar[1d]>   <rvar[1d]>
       1  -145 ± NA  -0.00055 ± NA  -152 ± NA    -152 ± NA
       2  -144 ± NA  -0.00207 ± NA  -150 ± NA    -150 ± NA
       3  -137 ± NA  -0.00498 ± NA  -143 ± NA    -143 ± NA
       4  -128 ± NA  -0.00831 ± NA  -134 ± NA    -134 ± NA
       5  -127 ± NA  -0.01111 ± NA  -133 ± NA    -133 ± NA
       6  -125 ± NA  -0.01320 ± NA  -132 ± NA    -131 ± NA
       7  -122 ± NA  -0.01410 ± NA  -130 ± NA    -130 ± NA
       8  -122 ± NA  -0.01437 ± NA  -128 ± NA    -128 ± NA
       9  -121 ± NA  -0.01737 ± NA  -127 ± NA    -127 ± NA
      10  -119 ± NA  -0.01830 ± NA  -126 ± NA    -126 ± NA
      # i 5,250 more rows

# calculate works when ndraws = 4000 (default)

    Code
      calc
    Output
      Nested sampling estimates <ernest_estimate>
      No. of Simulated Draws: 4000
      Log. Volume: -16 ± 1.3
      Log. Evidence: -9.2 ± 0.1
      # A tibble: 5,260 x 4
            log_lik       log_volume   log_weight log_evidence
         <rvar[1d]>       <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1  -145 ± NA  -0.002 ± 0.0020  -151 ± 0.83  -151 ± 0.83
       2  -144 ± NA  -0.004 ± 0.0028  -150 ± 0.82  -150 ± 0.67
       3  -137 ± NA  -0.006 ± 0.0034  -143 ± 0.82  -143 ± 0.82
       4  -128 ± NA  -0.008 ± 0.0040  -135 ± 0.82  -135 ± 0.82
       5  -127 ± NA  -0.010 ± 0.0045  -133 ± 0.81  -133 ± 0.70
       6  -125 ± NA  -0.012 ± 0.0049  -132 ± 0.79  -131 ± 0.67
       7  -122 ± NA  -0.014 ± 0.0053  -129 ± 0.81  -129 ± 0.74
       8  -122 ± NA  -0.016 ± 0.0057  -128 ± 0.80  -128 ± 0.64
       9  -121 ± NA  -0.018 ± 0.0060  -127 ± 0.79  -127 ± 0.58
      10  -119 ± NA  -0.020 ± 0.0064  -126 ± 0.79  -125 ± 0.60
      # i 5,250 more rows

