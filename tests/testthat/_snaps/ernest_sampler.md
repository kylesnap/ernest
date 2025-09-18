# ernest_sampler errors with invalid prior

    `prior` must be an object with class ernest_prior, not a function.

# Progress bar can be printed [plain]

    Code
      generate(sampler, max_iterations = 1, seed = 42, show_progress = TRUE)
    Output
      cli-line
      cli-line
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      * random walk in unit cube LRPS <rwmh_cube/ernest_lrps>
      * 
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1.000
      
      -- Results 
      No. Iterations: 1
      No. Calls: 0
      Log. Evidence: -10.50 (± 2.849)

# Progress bar can be printed [ansi]

    Code
      generate(sampler, max_iterations = 1, seed = 42, show_progress = TRUE)
    Output
      cli-line
      cli-line
      nested sampling results [34m<ernest_run/ernest_sampler>[39m
      
      No. Points: 500
      
      -- Sampling Method 
      * random walk in unit cube LRPS [34m<rwmh_cube/ernest_lrps>[39m
      * 
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1.000
      
      -- Results 
      No. Iterations: 1
      No. Calls: 0
      Log. Evidence: -10.50 (± 2.849)

# Progress bar can be printed [unicode]

    Code
      generate(sampler, max_iterations = 1, seed = 42, show_progress = TRUE)
    Output
      cli-line
      cli-line
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      ── Sampling Method 
      • random walk in unit cube LRPS <rwmh_cube/ernest_lrps>
      • 
      • No. Dimensions: 2
      • No. Calls Since Update: 0
      • No. Accepted Since Update: 0
      • Current Step Size: 1.000
      
      ── Results 
      No. Iterations: 1
      No. Calls: 0
      Log. Evidence: -10.50 (± 2.849)

# Progress bar can be printed [fancy]

    Code
      generate(sampler, max_iterations = 1, seed = 42, show_progress = TRUE)
    Output
      cli-line
      cli-line
      nested sampling results [34m<ernest_run/ernest_sampler>[39m
      
      No. Points: 500
      
      ── Sampling Method 
      • random walk in unit cube LRPS [34m<rwmh_cube/ernest_lrps>[39m
      • 
      • No. Dimensions: 2
      • No. Calls Since Update: 0
      • No. Accepted Since Update: 0
      • Current Step Size: 1.000
      
      ── Results 
      No. Iterations: 1
      No. Calls: 0
      Log. Evidence: -10.50 (± 2.849)

# Fully-verbose output [plain]

    Code
      generate(sampler, max_iterations = 1000, seed = 42, show_progress = FALSE)
    Message
      i Created 500 live points.
      v `max_iterations` reached (1000).
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      * random walk in unit cube LRPS <rwmh_cube/ernest_lrps>
      * 
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1.233
      
      -- Results 
      No. Iterations: 1000
      No. Calls: 5130
      Log. Evidence: -6.593 (± 1.590)

# Fully-verbose output [ansi]

    Code
      generate(sampler, max_iterations = 1000, seed = 42, show_progress = FALSE)
    Message
      [36mi[39m Created 500 live points.
      [32mv[39m `max_iterations` reached (1000).
    Output
      nested sampling results [34m<ernest_run/ernest_sampler>[39m
      
      No. Points: 500
      
      -- Sampling Method 
      * random walk in unit cube LRPS [34m<rwmh_cube/ernest_lrps>[39m
      * 
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1.233
      
      -- Results 
      No. Iterations: 1000
      No. Calls: 5130
      Log. Evidence: -6.593 (± 1.590)

# Fully-verbose output [unicode]

    Code
      generate(sampler, max_iterations = 1000, seed = 42, show_progress = FALSE)
    Message
      ℹ Created 500 live points.
      ✔ `max_iterations` reached (1000).
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      ── Sampling Method 
      • random walk in unit cube LRPS <rwmh_cube/ernest_lrps>
      • 
      • No. Dimensions: 2
      • No. Calls Since Update: 0
      • No. Accepted Since Update: 0
      • Current Step Size: 1.233
      
      ── Results 
      No. Iterations: 1000
      No. Calls: 5130
      Log. Evidence: -6.593 (± 1.590)

# Fully-verbose output [fancy]

    Code
      generate(sampler, max_iterations = 1000, seed = 42, show_progress = FALSE)
    Message
      [36mℹ[39m Created 500 live points.
      [32m✔[39m `max_iterations` reached (1000).
    Output
      nested sampling results [34m<ernest_run/ernest_sampler>[39m
      
      No. Points: 500
      
      ── Sampling Method 
      • random walk in unit cube LRPS [34m<rwmh_cube/ernest_lrps>[39m
      • 
      • No. Dimensions: 2
      • No. Calls Since Update: 0
      • No. Accepted Since Update: 0
      • Current Step Size: 1.233
      
      ── Results 
      No. Iterations: 1000
      No. Calls: 5130
      Log. Evidence: -6.593 (± 1.590)

# set_logging enables and disables logging

    Code
      generate(sampler, max_iterations = 1000, seed = 42, show_progress = FALSE)
    Message
      i Logging run at FILE.
    Output
      nested sampling results <ernest_run/ernest_sampler>
      
      No. Points: 500
      
      -- Sampling Method 
      * random walk in unit cube LRPS <rwmh_cube/ernest_lrps>
      * 
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1.233
      
      -- Results 
      No. Iterations: 1000
      No. Calls: 5130
      Log. Evidence: -6.593 (± 1.590)

