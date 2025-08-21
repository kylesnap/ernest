# ernest_sampler errors with invalid prior

    `prior` must be an object with class ernest_prior, not a function.

# Progress bar can be printed [plain]

    Code
      generate(sampler, max_iterations = 1, seed = 42L, show_progress = TRUE)
    Output
      cli-69638-21 0/NA created
      cli-69638-21 1/NA added
      cli-69638-21 1/NA updated
      cli-69638-21 2/NA terminated (done)
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
      No. Calls: 1
      Log. Evidence: -2.03419130597453 (± 0.785672016773844)

# Progress bar can be printed [ansi]

    Code
      generate(sampler, max_iterations = 1, seed = 42L, show_progress = TRUE)
    Output
      cli-69638-60 0/NA created
      cli-69638-60 1/NA added
      cli-69638-60 1/NA updated
      cli-69638-60 2/NA terminated (done)
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
      No. Calls: 1
      Log. Evidence: [34m-2.03419130597453[39m (± [34m0.785672016773844[39m)

# Progress bar can be printed [unicode]

    Code
      generate(sampler, max_iterations = 1, seed = 42L, show_progress = TRUE)
    Output
      cli-69638-99 0/NA created
      cli-69638-99 1/NA added
      cli-69638-99 1/NA updated
      cli-69638-99 2/NA terminated (done)
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
      No. Calls: 1
      Log. Evidence: -2.03419130597453 (± 0.785672016773844)

# Progress bar can be printed [fancy]

    Code
      generate(sampler, max_iterations = 1, seed = 42L, show_progress = TRUE)
    Output
      cli-69638-138 0/NA created
      cli-69638-138 1/NA added
      cli-69638-138 1/NA updated
      cli-69638-138 2/NA terminated (done)
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
      No. Calls: 1
      Log. Evidence: [34m-2.03419130597453[39m (± [34m0.785672016773844[39m)

# Fully-verbose output [plain]

    Code
      generate(sampler, max_iterations = 1000, seed = 42L, show_progress = FALSE)
    Message
      Creating new live points...
      `max_iterations` reached (1000).
    Output
      Nested sampling run <ernest_run/ernest_sampler>
      No. Points: 500
      
      -- Sampling Method 
      * Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 0.0981878434301509
      
      -- Results 
      No. Iterations: 1000
      No. Calls: 10984
      Log. Evidence: -1.79587595132713 (± 0.378275783151236)

# Fully-verbose output [ansi]

    Code
      generate(sampler, max_iterations = 1000, seed = 42L, show_progress = FALSE)
    Message
      [1m[22mCreating new live points...
      [1m[22m`max_iterations` reached (1000).
    Output
      Nested sampling run [34m<ernest_run/ernest_sampler>[39m
      No. Points: 500
      
      -- Sampling Method 
      * Random Walk in Unit Cube LRPS [34m<rwmh_cube/ernest_lrps>[39m
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 0.0981878434301509
      
      -- Results 
      No. Iterations: 1000
      No. Calls: 10984
      Log. Evidence: [34m-1.79587595132713[39m (± [34m0.378275783151236[39m)

# Fully-verbose output [unicode]

    Code
      generate(sampler, max_iterations = 1000, seed = 42L, show_progress = FALSE)
    Message
      Creating new live points...
      `max_iterations` reached (1000).
    Output
      Nested sampling run <ernest_run/ernest_sampler>
      No. Points: 500
      
      ── Sampling Method 
      • Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      • No. Dimensions: 2
      • No. Calls Since Update: 0
      • No. Accepted Since Update: 0
      • Current Step Size: 0.0981878434301509
      
      ── Results 
      No. Iterations: 1000
      No. Calls: 10984
      Log. Evidence: -1.79587595132713 (± 0.378275783151236)

# Fully-verbose output [fancy]

    Code
      generate(sampler, max_iterations = 1000, seed = 42L, show_progress = FALSE)
    Message
      [1m[22mCreating new live points...
      [1m[22m`max_iterations` reached (1000).
    Output
      Nested sampling run [34m<ernest_run/ernest_sampler>[39m
      No. Points: 500
      
      ── Sampling Method 
      • Random Walk in Unit Cube LRPS [34m<rwmh_cube/ernest_lrps>[39m
      • No. Dimensions: 2
      • No. Calls Since Update: 0
      • No. Accepted Since Update: 0
      • Current Step Size: 0.0981878434301509
      
      ── Results 
      No. Iterations: 1000
      No. Calls: 10984
      Log. Evidence: [34m-1.79587595132713[39m (± [34m0.378275783151236[39m)

