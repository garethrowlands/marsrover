
module Test.EnvironmentTests

where

import Test.HUnit
import Environment

-- |Tests 'outside' boundary conditions with various (valid) sizes of plateau.
outsideTests :: Test
outsideTests = "outside" ~: test [
        show p ~:
        test [
            "-1,-1 is outside" ~: (-1,-1) `outside` p ~=? True,
            "0,0 is inside" ~: (0,0) `outside` p ~=? False,
            "right,top is inside" ~: (r,t) `outside` p ~=? False,
            "right+1,top is outside" ~: (r+1,t) `outside` p ~=? True,
            "right,top+1 is outside" ~: (r,t+1) `outside` p ~=? True,
            "right+1,top+1 is outside" ~: (r+1,t+1) `outside` p ~=? True
        ] |
        r <- [0..5],
        t <- [0,10..100],
        let Right p = mkPlateau r t
    ]

allEnvironmentTests = "environment tests" ~: test [
        outsideTests
    ]

