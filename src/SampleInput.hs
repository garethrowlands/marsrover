module SampleInput
where

  -- |The input provided by the specification.
  -- (Implemented as a list of strings rather than a multiline string with back slashes)
  overallInput :: String
  overallInput = unlines [
          "5 5",
          "1 2 N",
          "LMLMLMLMM",
          "3 3 E",
          "MMRMMRMRRM"
      ]
