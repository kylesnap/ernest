# crosschecks work and are repeatable

    Code
      crosscheck_tests(example_run)
    Output
      # A tibble: 9 x 4
        split        statistic.W2 p.value adj.p.value
        <chr>               <dbl>   <dbl>       <dbl>
      1 [1, 1000]          0.0542  0.850        1    
      2 [1001, 2000]       0.423   0.0627       0.502
      3 [2001, 3000]       0.290   0.144        1    
      4 [3001, 4000]       0.0857  0.660        1    
      5 [4001, 5000]       0.110   0.536        1    
      6 [5001, 6000]       0.539   0.0318       0.286
      7 [6001, 7000]       0.135   0.439        1    
      8 [7001, 8000]       0.114   0.520        1    
      9 [8001, 9000]       0.0546  0.848        1    

