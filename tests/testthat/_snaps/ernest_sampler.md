# ernest_sampler errors with invalid prior

    `prior` must be an object with class ernest_prior, not a function.

# Progress bar can be printed [plain]

    Code
      generate(sampler, max_iterations = 1, seed = 42, show_progress = TRUE)
    Output
      cli-line
      cli-line
      cli-line
      cli-line
      Nested sampling run <ernest_run/ernest_sampler>
      No. Points: 500
      
      -- Sampling Method 
      * Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1
      
      -- Results 
      No. Iterations: 1
      No. Calls: 0
      Log. Evidence: -10.5042 (± 2.8489)

# Progress bar can be printed [ansi]

    Code
      generate(sampler, max_iterations = 1, seed = 42, show_progress = TRUE)
    Output
      cli-line
      cli-line
      cli-line
      cli-line
      Nested sampling run [34m<ernest_run/ernest_sampler>[39m
      No. Points: 500
      
      -- Sampling Method 
      * Random Walk in Unit Cube LRPS [34m<rwmh_cube/ernest_lrps>[39m
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1
      
      -- Results 
      No. Iterations: 1
      No. Calls: 0
      Log. Evidence: -10.5042 (± 2.8489)

# Progress bar can be printed [unicode]

    Code
      generate(sampler, max_iterations = 1, seed = 42, show_progress = TRUE)
    Output
      cli-line
      cli-line
      cli-line
      cli-line
      Nested sampling run <ernest_run/ernest_sampler>
      No. Points: 500
      
      ── Sampling Method 
      • Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      • No. Dimensions: 2
      • No. Calls Since Update: 0
      • No. Accepted Since Update: 0
      • Current Step Size: 1
      
      ── Results 
      No. Iterations: 1
      No. Calls: 0
      Log. Evidence: -10.5042 (± 2.8489)

# Progress bar can be printed [fancy]

    Code
      generate(sampler, max_iterations = 1, seed = 42, show_progress = TRUE)
    Output
      cli-line
      cli-line
      cli-line
      cli-line
      Nested sampling run [34m<ernest_run/ernest_sampler>[39m
      No. Points: 500
      
      ── Sampling Method 
      • Random Walk in Unit Cube LRPS [34m<rwmh_cube/ernest_lrps>[39m
      • No. Dimensions: 2
      • No. Calls Since Update: 0
      • No. Accepted Since Update: 0
      • Current Step Size: 1
      
      ── Results 
      No. Iterations: 1
      No. Calls: 0
      Log. Evidence: -10.5042 (± 2.8489)

# Fully-verbose output [plain]

    Code
      generate(sampler, max_iterations = 1000, seed = 42, show_progress = FALSE)
    Message
      v Creating new live points.
      v `max_iterations` reached (1000).
    Output
      Nested sampling run <ernest_run/ernest_sampler>
      No. Points: 500
      
      -- Sampling Method 
      * Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1.2333
      
      -- Results 
      No. Iterations: 1000
      No. Calls: 5130
      Log. Evidence: -6.5929 (± 1.59)

# Fully-verbose output [ansi]

    Code
      generate(sampler, max_iterations = 1000, seed = 42, show_progress = FALSE)
    Message
      [1m[22m[32mv[39m Creating new live points.
      [1m[22m[32mv[39m `max_iterations` reached (1000).
    Output
      Nested sampling run [34m<ernest_run/ernest_sampler>[39m
      No. Points: 500
      
      -- Sampling Method 
      * Random Walk in Unit Cube LRPS [34m<rwmh_cube/ernest_lrps>[39m
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1.2333
      
      -- Results 
      No. Iterations: 1000
      No. Calls: 5130
      Log. Evidence: -6.5929 (± 1.59)

# Fully-verbose output [unicode]

    Code
      generate(sampler, max_iterations = 1000, seed = 42, show_progress = FALSE)
    Message
      ✔ Creating new live points.
      ✔ `max_iterations` reached (1000).
    Output
      Nested sampling run <ernest_run/ernest_sampler>
      No. Points: 500
      
      ── Sampling Method 
      • Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      • No. Dimensions: 2
      • No. Calls Since Update: 0
      • No. Accepted Since Update: 0
      • Current Step Size: 1.2333
      
      ── Results 
      No. Iterations: 1000
      No. Calls: 5130
      Log. Evidence: -6.5929 (± 1.59)

# Fully-verbose output [fancy]

    Code
      generate(sampler, max_iterations = 1000, seed = 42, show_progress = FALSE)
    Message
      [1m[22m[32m✔[39m Creating new live points.
      [1m[22m[32m✔[39m `max_iterations` reached (1000).
    Output
      Nested sampling run [34m<ernest_run/ernest_sampler>[39m
      No. Points: 500
      
      ── Sampling Method 
      • Random Walk in Unit Cube LRPS [34m<rwmh_cube/ernest_lrps>[39m
      • No. Dimensions: 2
      • No. Calls Since Update: 0
      • No. Accepted Since Update: 0
      • Current Step Size: 1.2333
      
      ── Results 
      No. Iterations: 1000
      No. Calls: 5130
      Log. Evidence: -6.5929 (± 1.59)

