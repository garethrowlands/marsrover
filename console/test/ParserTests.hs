-- |Tests for the "Parser" module
module Test.ParserTests

where

import Commands
import Environment
import Geometry
import Parser
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos (newPos)

-- |Run the given parser against the given 'String', returning
-- 'Either' the successfully parsed value or the 'String' part of the error message.
-- Trims any trailing newlines off the error.
parseStr :: CharParser () a -> String -> Either String a
parseStr p input = case parse p "" input of
                Left parseError -> Left . trimNewlines . show $ parseError
                Right parsedValue -> Right parsedValue

-- |Removes any trailing newlines
trimNewlines :: String -> String
trimNewlines = reverse . dropWhile (== '\n') . reverse

-- |Builds up a test suite for the given parser using the supplied list of
-- pairs of input string and expected parsed values. Labels the suite with
-- the supplied label and the individual tests with labels "1", "2"...
mkParserTests label parser params = label ~: test [
        "parsing '"++input++"'" ~: expected ~=? parseStr parser input | (input,expected) <- params
    ]


-- |Test parsing plateau of various sizes.
-- Notice how I don't test any difficult sizes, such as 0 or negative.
-- Uses a list comprehension and string concatenation to generate the data.
parsePlateauTests = mkParserTests "plateau" p_plateau [
        (show width ++ " " ++ show height, Right (mkPlateau width height)) |
        width<-[1,10..100], height<-[1..5]
    ]

-- |Test parsing of each possible command
parseCommandTests :: Test
parseCommandTests = mkParserTests "command" p_command [
            ("L", Right (Turn AntiClockwise)),
            ("R", Right (Turn Clockwise)),
            ("M", Right Forwards),
            ("MM", Right Forwards)
    ]

-- |Test parsing of each possible 'Heading'.
-- Checks that unrecognised input gives a decent error message.
parseDirectionTests :: Test
parseDirectionTests = mkParserTests "direction" p_heading [
        ("N", Right N),
        ("E", Right E),
        ("S", Right S),
        ("W", Right W),
        ("X", Left . trimNewlines . unlines $ [
            "(line 1, column 1):",
            "unexpected \"X\"",
            "expecting \"N\", \"E\", \"S\" or \"W\""])
    ]

-- |Test parsing of the line that defines a 'Rover'
parseRoverTests :: Test
parseRoverTests = mkParserTests "rover " p_rover [
        ("1 1 N", Right (mkRover (1,1) N)),
        ("1 2 E", Right (mkRover (1,2) E)),
        ("!1 2 E", Left "(line 1, column 1):\nunexpected \"!\"\nexpecting integer")
    ]

-- |Test parsing of the two lines that define a 'Rover' and its input
parseRoverInputTests :: Test
parseRoverInputTests = "input for a single rover" ~: mkParserTests "rover" p_roverInput [
        ("1 1 N\nLRM",
         Right $ RoverInput {
            inputRover = mkRover (1,1) N,
            inputCommands = [Turn AntiClockwise,Turn Clockwise,Forwards]
         })
    ]

-- |Check the input provided by the specification
parseOverallInputTest :: Test
parseOverallInputTest = "parse overall input" ~:
    Right parsedOverallInput ~=? parseStr p_overallInput overallInput

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

-- |The parsed representation of the input provided in the specification
parsedOverallInput :: OverallInput
parsedOverallInput =
    OverallInput {
        inputPlateau = mkPlateau 5 5,
        inputRovers = [
            RoverInput {
                inputRover = mkRover (1,2) N,
                inputCommands = [Turn AntiClockwise,
                                 Forwards,
                                 Turn AntiClockwise,
                                 Forwards,
                                 Turn AntiClockwise,
                                 Forwards,
                                 Turn AntiClockwise,
                                 Forwards,Forwards]
            },
            RoverInput {
                inputRover = mkRover (3,3) E,
                inputCommands = [Forwards,
                                 Forwards,
                                 Turn Clockwise,
                                 Forwards,
                                 Forwards,
                                 Turn Clockwise,
                                 Forwards,
                                 Turn Clockwise,
                                 Turn Clockwise,
                                 Forwards]
            }
        ]
    }

-- |Combined test suite for parsing
parserTests :: Test
parserTests = "parser tests" ~: test [
        parsePlateauTests,
        parseCommandTests,
        parseDirectionTests,
        parseRoverTests,
        parseRoverInputTests,
        parseOverallInputTest
    ]

