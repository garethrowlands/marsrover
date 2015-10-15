-- | Main entry point to test modules
module Test
    (
        -- * Running Tests
        printAllTests,
        -- * Re-exported from Other Modules
        overallInput
    )
where

import Test.HUnit
import ParserTests
import GeometryTests
import EnvironmentTests
import RenderTests
import SampleInput


-- |Suite containing all tests
allTests :: Test
allTests = test [
        parserTests,
        allNavigationTests,
        allEnvironmentTests,
        allRenderTests
    ]

-- |Runs all tests and prints results
printAllTests :: IO ()
printAllTests = do (c,st) <- runTestText putTextToShowS $ allTests
                   print c
                   putStrLn (st "")
