# calculate works when ndraws = 0

    Code
      calc
    Output
      Nested sampling estimates <ernest_estimate>
      Log. Volume: -16 ± NA
      Log. Evidence: -8.9 ± NA
      # A tibble: 5,136 x 5
            log_lik   log_volume log_weight log_evidence log_evidence_err
         <rvar[1d]>   <rvar[1d]> <rvar[1d]>   <rvar[1d]>       <rvar[1d]>
       1  -146 ± NA  -0.002 ± NA  -152 ± NA    -152 ± NA     2.7e-32 ± NA
       2  -144 ± NA  -0.004 ± NA  -150 ± NA    -150 ± NA     8.1e-32 ± NA
       3  -139 ± NA  -0.006 ± NA  -145 ± NA    -145 ± NA     1.0e-30 ± NA
       4  -136 ± NA  -0.008 ± NA  -142 ± NA    -142 ± NA     5.1e-30 ± NA
       5  -129 ± NA  -0.010 ± NA  -135 ± NA    -135 ± NA     1.4e-28 ± NA
       6  -126 ± NA  -0.012 ± NA  -133 ± NA    -133 ± NA     5.1e-28 ± NA
       7  -125 ± NA  -0.014 ± NA  -131 ± NA    -131 ± NA     1.2e-27 ± NA
       8  -125 ± NA  -0.016 ± NA  -131 ± NA    -130 ± NA     1.9e-27 ± NA
       9  -118 ± NA  -0.018 ± NA  -124 ± NA    -124 ± NA     2.6e-26 ± NA
      10  -118 ± NA  -0.020 ± NA  -124 ± NA    -124 ± NA     4.9e-26 ± NA
      # i 5,126 more rows

# calculate works when ndraws = 1

    Code
      calc
    Output
      Nested sampling estimates <ernest_estimate>
      No. of Simulated Draws: 1
      Log. Volume: -16 ± NA
      Log. Evidence: -9.1 ± NA
      # A tibble: 5,136 x 4
            log_lik     log_volume log_weight log_evidence
         <rvar[1d]>     <rvar[1d]> <rvar[1d]>   <rvar[1d]>
       1  -146 ± NA  -0.00055 ± NA  -153 ± NA    -153 ± NA
       2  -144 ± NA  -0.00207 ± NA  -150 ± NA    -150 ± NA
       3  -139 ± NA  -0.00498 ± NA  -144 ± NA    -144 ± NA
       4  -136 ± NA  -0.00831 ± NA  -141 ± NA    -141 ± NA
       5  -129 ± NA  -0.01111 ± NA  -135 ± NA    -135 ± NA
       6  -126 ± NA  -0.01320 ± NA  -133 ± NA    -133 ± NA
       7  -125 ± NA  -0.01410 ± NA  -132 ± NA    -132 ± NA
       8  -125 ± NA  -0.01437 ± NA  -131 ± NA    -131 ± NA
       9  -118 ± NA  -0.01737 ± NA  -124 ± NA    -124 ± NA
      10  -118 ± NA  -0.01830 ± NA  -125 ± NA    -124 ± NA
      # i 5,126 more rows

# calculate works when ndraws = 4000 (default)

    Code
      calc
    Output
      Nested sampling estimates <ernest_estimate>
      No. of Simulated Draws: 4000
      Log. Volume: -16 ± 1.3
      Log. Evidence: -8.9 ± 0.1
      # A tibble: 5,136 x 4
            log_lik       log_volume   log_weight log_evidence
         <rvar[1d]>       <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
       1  -146 ± NA  -0.002 ± 0.0020  -152 ± 0.83  -152 ± 0.83
       2  -144 ± NA  -0.004 ± 0.0028  -151 ± 0.82  -150 ± 0.68
       3  -139 ± NA  -0.006 ± 0.0034  -145 ± 0.81  -145 ± 0.81
       4  -136 ± NA  -0.008 ± 0.0040  -142 ± 0.82  -142 ± 0.78
       5  -129 ± NA  -0.010 ± 0.0045  -135 ± 0.81  -135 ± 0.81
       6  -126 ± NA  -0.012 ± 0.0049  -133 ± 0.79  -133 ± 0.73
       7  -125 ± NA  -0.014 ± 0.0053  -131 ± 0.81  -131 ± 0.68
       8  -125 ± NA  -0.016 ± 0.0057  -131 ± 0.80  -130 ± 0.60
       9  -118 ± NA  -0.018 ± 0.0061  -125 ± 0.79  -125 ± 0.79
      10  -118 ± NA  -0.020 ± 0.0064  -124 ± 0.79  -124 ± 0.65
      # i 5,126 more rows

