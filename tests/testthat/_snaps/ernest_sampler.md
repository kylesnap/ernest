# ernest_sampler errors with invalid prior

    `prior` must be an object with class ernest_prior, not a function.

# Progress bar can be printed [plain]

    Code
      generate(sampler, max_iterations = 1)
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "No. Accepted Proposals: 0" "No. Steps: 25"            
      [3] "Target Acceptance: 0.5"    "Step Size: 1.000"         
      
      -- Results 
      No. Iterations: 1
      No. Calls: 1
      Log. Evidence: -7.557 (± 2.432)

# Progress bar can be printed [ansi]

    Code
      generate(sampler, max_iterations = 1)
    Output
      nested sampling results [34m<ernest_run/ernest_sampler>[39m
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "No. Accepted Proposals: 0" "No. Steps: 25"            
      [3] "Target Acceptance: 0.5"    "Step Size: 1.000"         
      
      -- Results 
      No. Iterations: 1
      No. Calls: 1
      Log. Evidence: -7.440 (± 2.369)

# Progress bar can be printed [unicode]

    Code
      generate(sampler, max_iterations = 1)
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      ── Sampling Method 
      [1] "No. Accepted Proposals: 0" "No. Steps: 25"            
      [3] "Target Acceptance: 0.5"    "Step Size: 1.000"         
      
      ── Results 
      No. Iterations: 1
      No. Calls: 1
      Log. Evidence: -6.443 (± 2.029)

# Progress bar can be printed [fancy]

    Code
      generate(sampler, max_iterations = 1)
    Output
      nested sampling results [34m<ernest_run/ernest_sampler>[39m
      
      No. Points: 500
      
      ── Sampling Method 
      [1] "No. Accepted Proposals: 0" "No. Steps: 25"            
      [3] "Target Acceptance: 0.5"    "Step Size: 1.000"         
      
      ── Results 
      No. Iterations: 1
      No. Calls: 1
      Log. Evidence: -6.209 (± 2.042)

# Fully-verbose output [plain]

    Code
      generate(sampler, max_iterations = 1000)
    Message
      i Created 500 live points.
      v `max_iterations` reached (1000).
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "No. Accepted Proposals: 0" "No. Steps: 25"            
      [3] "Target Acceptance: 0.5"    "Step Size: 1.000"         
      
      -- Results 
      No. Iterations: 1000
      No. Calls: 11036
      Log. Evidence: -6.621 (± 1.473)

# Fully-verbose output [ansi]

    Code
      generate(sampler, max_iterations = 1000)
    Message
      [36mi[39m Created 500 live points.
      [32mv[39m `max_iterations` reached (1000).
    Output
      nested sampling results [34m<ernest_run/ernest_sampler>[39m
      
      No. Points: 500
      
      -- Sampling Method 
      [1] "No. Accepted Proposals: 0" "No. Steps: 25"            
      [3] "Target Acceptance: 0.5"    "Step Size: 1.000"         
      
      -- Results 
      No. Iterations: 1000
      No. Calls: 9902
      Log. Evidence: -6.438 (± 1.424)

# Fully-verbose output [unicode]

    Code
      generate(sampler, max_iterations = 1000)
    Message
      ℹ Created 500 live points.
      ✔ `max_iterations` reached (1000).
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      ── Sampling Method 
      [1] "No. Accepted Proposals: 0" "No. Steps: 25"            
      [3] "Target Acceptance: 0.5"    "Step Size: 1.000"         
      
      ── Results 
      No. Iterations: 1000
      No. Calls: 10376
      Log. Evidence: -7.188 (± 1.807)

# Fully-verbose output [fancy]

    Code
      generate(sampler, max_iterations = 1000)
    Message
      [36mℹ[39m Created 500 live points.
      [32m✔[39m `max_iterations` reached (1000).
    Output
      nested sampling results [34m<ernest_run/ernest_sampler>[39m
      
      No. Points: 500
      
      ── Sampling Method 
      [1] "No. Accepted Proposals: 0" "No. Steps: 25"            
      [3] "Target Acceptance: 0.5"    "Step Size: 1.000"         
      
      ── Results 
      No. Iterations: 1000
      No. Calls: 10301
      Log. Evidence: -6.670 (± 1.672)

