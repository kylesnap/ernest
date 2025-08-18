# calculate works when ndraws = 0

    Code
      calc
    Output
      Nested sampling estimates <ernest_estimate>
      Log. Volume: -16 ± NA
      Log. Evidence: -9 ± NA
      # A tibble: 5,158 x 5
            log_lik   log_volume log_weight log_evidence log_evidence_err
         <rvar[1d]>   <rvar[1d]> <rvar[1d]>   <rvar[1d]>       <rvar[1d]>
       1  -146 ± NA  -0.002 ± NA  -153 ± NA    -153 ± NA     3.0e-32 ± NA
       2  -144 ± NA  -0.004 ± NA  -151 ± NA    -151 ± NA     8.5e-32 ± NA
       3  -139 ± NA  -0.006 ± NA  -146 ± NA    -146 ± NA     1.1e-30 ± NA
       4  -136 ± NA  -0.008 ± NA  -142 ± NA    -142 ± NA     5.4e-30 ± NA
       5  -129 ± NA  -0.010 ± NA  -136 ± NA    -136 ± NA     1.5e-28 ± NA
       6  -126 ± NA  -0.012 ± NA  -133 ± NA    -133 ± NA     5.4e-28 ± NA
       7  -125 ± NA  -0.014 ± NA  -132 ± NA    -131 ± NA     1.2e-27 ± NA
       8  -125 ± NA  -0.016 ± NA  -131 ± NA    -130 ± NA     2.0e-27 ± NA
       9  -118 ± NA  -0.018 ± NA  -125 ± NA    -125 ± NA     2.8e-26 ± NA
      10  -118 ± NA  -0.020 ± NA  -124 ± NA    -124 ± NA     5.1e-26 ± NA
      # i 5,148 more rows

# calculate works when ndraws = 1

    Code
      calc
    Output
      Nested sampling estimates <ernest_estimate>
      No. of Simulated Draws: 1
      Log. Volume: -16 ± NA
      Log. Evidence: -9 ± NA
      # A tibble: 5,158 x 4
            log_lik     log_volume log_weight log_evidence
         <rvar[1d]>     <rvar[1d]> <rvar[1d]>   <rvar[1d]>
       1  -146 ± NA  -0.00025 ± NA  -155 ± NA    -155 ± NA
       2  -144 ± NA  -0.00131 ± NA  -152 ± NA    -151 ± NA
       3  -139 ± NA  -0.00373 ± NA  -145 ± NA    -145 ± NA
       4  -136 ± NA  -0.00427 ± NA  -144 ± NA    -144 ± NA
       5  -129 ± NA  -0.00636 ± NA  -136 ± NA    -136 ± NA
       6  -126 ± NA  -0.00930 ± NA  -133 ± NA    -133 ± NA
       7  -125 ± NA  -0.00980 ± NA  -133 ± NA    -132 ± NA
       8  -125 ± NA  -0.01016 ± NA  -133 ± NA    -132 ± NA
       9  -118 ± NA  -0.01026 ± NA  -128 ± NA    -128 ± NA
      10  -118 ± NA  -0.01142 ± NA  -125 ± NA    -125 ± NA
      # i 5,148 more rows

