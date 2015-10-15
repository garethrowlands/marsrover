-- |Contains the main program. Depending on command-line arguments, can:
--  * Run interactively, reading stdin and writing stdout
--  * Run the sample input, producing the sample output
--  * Parse the sample input and print a representation of the parsed result
--
module Main (

    main

) where

import System.Environment (getArgs)

import Parser
import Render
import Controller
import Geometry
import Environment
import SampleInput

run s = case parseInput s of
             Left err -> show err
             Right parsedInput -> render . runOverallInput $ parsedInput


printHelpMessage = putStrLn . unlines $ [
        "rover --interactive",
        "    accept rover input on standard input and print output to standard out",
        "rover --run",
        "    run the sample rover input and print the results to standard out",
        "rover --parse",
        "    accept rover input on standard input and show parsed version on standard out"
    ]

main = do args <- getArgs
          case args of
            ["--interactive"] -> interact run
            ["--run"]         -> putStr $ run overallInput
            ["--parse"]       -> print $ parseInput overallInput
            _                 -> printHelpMessage
