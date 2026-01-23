# ernest_run / Stores weights as list

    Code
      example_run
    Message
      Nested sampling run:
      * No. points: 500
      * Sampling method: 25-step random walk sampling (acceptance target = 50.0%)
      * Prior: uniform prior distribution with 2 dimensions (A and B)
      -- Results ---------------------------------------------------------------------
      * Iterations: 1000
      * Likelihood evals.: 11203
      * Log-evidence: -7.1156 (± 1.9058)
      * Information: 6.165

# summary.ernest_run returns expected structure and values / has the expected posterior

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["variable", "mean", "sd", "median", "q15", "q85"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["draws_summary", "tbl_df", "tbl", "data.frame"]
        },
        "num_args": {
          "type": "list",
          "attributes": {},
          "value": []
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["A", "B"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.05652933, -0.04601343]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.28723902, 2.34610747]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.03116833, -0.08325333]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-2.59695916, -2.58772456]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.38923974, 2.58079068]
        }
      ]
    }

