module ParserSpec where


import Commands
import Environment
import Geometry
import Parser
import Test.HUnit
import Text.ParserCombinators.Parsec

import Test.Hspec
import Control.Monad (forM_)

spec :: Spec
spec = do
        parsePlateauSpec
        parseCommandSpec
        parseDirectionSpec
        parseLocationSpec
        parseRoverSpec
        parseRoverInputSpec
        parseOverallInputSpec


-- |Test parsing plateau of various sizes.
-- Notice how I don't test any difficult sizes, such as 0 or negative.
-- Uses a list comprehension and string concatenation to generate the data.
parsePlateauSpec :: Spec
parsePlateauSpec = mkParserSpec "plateau" p_plateau [
        (show width ++ " " ++ show height, Right (mkPlateau width height)) |
        width<- [-1,9,10],
        height<-[-1,5000]
    ]

-- |Test parsing of each possible command
parseCommandSpec :: Spec
parseCommandSpec = mkParserSpec "parsing a Command" p_command [
            ("L", Right (Turn AntiClockwise)),
            ("R", Right (Turn Clockwise)),
            ("M", Right Forwards),
            ("MM", Right Forwards) -- we only parse the first character
    ]

-- |Test parsing of each possible 'Heading'.
-- Checks that unrecognised input gives a decent error message.
parseDirectionSpec :: Spec
parseDirectionSpec = mkParserSpec "direction" p_heading [
        ("N", Right N),
        ("E", Right E),
        ("S", Right S),
        ("W", Right W),
        ("X", Left . trimNewlines . unlines $ [
            "(line 1, column 1):",
            "unexpected \"X\"",
            "expecting \"N\", \"E\", \"S\" or \"W\""])
    ]

parseLocationSpec :: Spec
parseLocationSpec = mkParserSpec "location" p_location [
        ("1 1", Right (1,1)),
        ("10 -10", Right (10,-10))
    ]

-- |Test parsing of the line that defines a 'Rover'
parseRoverSpec :: Spec
parseRoverSpec = mkParserSpec "rover " p_rover [
        ("1 1 N", Right (mkRover (1,1) N)),
        ("1 2 E", Right (mkRover (1,2) E)),
        ("!1 2 E", Left "(line 1, column 1):\nunexpected \"!\"\nexpecting integer")
    ]

-- |Test parsing of the two lines that define a 'Rover' and its input
parseRoverInputSpec :: Spec
parseRoverInputSpec = mkParserSpec "input for a single rover" p_roverInput [
        ("1 1 N\nLRM",
         Right RoverInput {
            inputRover = mkRover (1,1) N,
            inputCommands = [Turn AntiClockwise,Turn Clockwise,Forwards]
         })
    ]

-- |Check the input provided by the specification
parseOverallInputSpec :: Spec
parseOverallInputSpec = mkParserSpec "complete acceptance test input" p_overallInput [
        (overallInput, Right parsedOverallInput) 
    ]

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
-- the supplied label and the individual Spec with labels "1", "2"...
mkParserTests :: (Eq a, Show a) => String -> CharParser () a -> [(String, Either String a)] -> Test
mkParserTests parserName parser params = parserName ~: test [
        "parsing "++ show input ++" "++ showParseResult expected ~:
        expected ~=? parseStr parser input | (input,expected) <- params
    ]
    where
        showParseResult (Left err) = "fails with " ++ show err
        showParseResult (Right res) = "succeeds with " ++ show res

mkParserSpec :: (Eq a, Show a) => String -> CharParser () a -> [(String, Either String a)] -> Spec
mkParserSpec parserName parser pairs = describe parserName $ do
    forM_ pairs $ \(input, expected) ->
        it ("parsing "++ show input ++" "++ showParseResult expected) $
            expected ~=? parseStr parser input
    where
        showParseResult (Left err) = "fails with " ++ show err
        showParseResult (Right res) = "succeeds with " ++ show res


