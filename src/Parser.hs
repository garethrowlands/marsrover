{-# LANGUAGE TypeSynonymInstances #-}
-- | Parses the input string into Haskell data.
--   Uses a parser combinator library called "Parsec",
--   which is commonly used in production code.
--   "Parsec" parsers produce good error messages
--   when given malformed input.
--   The style in which it's used is known as applicative -
--   compare 'p_location' and 'p_location_monadStyle' to
--   contrast applicative and monadic styles.
module Parser
where

import Commands
import Geometry
import Environment
import Text.ParserCombinators.Parsec hiding (optional)
import Control.Applicative hiding ((<|>))

-- |Returns the parsed 'OverallInput' from the input string.
--  Helper function that just runs the parser against the input.
parseInput :: String -> Either ParseError OverallInput
parseInput input = parse p_overallInput "(test string)" input

-- |A 'Plateau' of the given width and height (or 'PlateauError' if dimensions are bad)
p_plateau :: CharParser st PlateauOrError
p_plateau = mkPlateau <$> p_int <* p_whiteSpace <*> p_int

-- |One or more whitespace characters
p_whiteSpace :: CharParser st String
p_whiteSpace = many1 $ oneOf [' ','\t']


-- |End of line (with possible leading whitespace)
p_newLine :: CharParser st Char
p_newLine = optional p_whiteSpace *> newline

-- |A 'Rover' with its 'Location' and 'CompassDirection'
p_rover :: CharParser st RoverPos
p_rover = mkRover <$> p_location <* p_whiteSpace <*> p_heading

-- |Tuple up two integers. Written in applicative style.
p_location :: CharParser st Location
p_location = (,) <$> p_int <* p_whiteSpace <*> p_int

-- |Same as 'p_location' but using monadic style.
-- (For parsers, monadic style is longer than applicative style
-- but the named values (@x@ and @y@ in this case) can make
-- the code clearer, especially for readers who aren't used to
-- an applicative style.)
p_location_monadStyle :: CharParser st Location
p_location_monadStyle = do x <- p_int
                           p_whiteSpace
                           y <- p_int
                           return (x,y)

-- |A compass direction
p_heading :: CharParser st Heading
p_heading = N <$ char 'N'
        <|> E <$ char 'E'
        <|> S <$ char 'S'
        <|> W <$ char 'W'

-- |A 'Command'
p_command :: CharParser st Command
p_command = constChar (Turn AntiClockwise) 'L'
        <|> constChar (Turn Clockwise) 'R'
        <|> constChar Forwards 'M'

-- |A parser that reads the given character and returns the given value
constChar :: a -> Char -> CharParser st a
constChar val ch = const val <$> char ch

-- |Parses an 'Int'. Doesn't allow leading minus sign. Just reads a string of digits
--  and calls the built-in function to convert them to an 'Int'.
p_int :: CharParser st Int
p_int = read <$> s <?> "integer"
    where
        s = do
            maybeMinus <- optional $ char '-'
            digits <- many1 digit
            return $ maybe digits (: digits) maybeMinus


-- |The input for a single rover
p_roverInput :: CharParser st RoverInput
p_roverInput = RoverInput <$> p_rover <* p_newLine <*> many1 p_command

-- |Parses the overall input
p_overallInput :: CharParser st OverallInput
p_overallInput = OverallInput <$> p_plateau <* p_newLine <*> p_roverInput `sepEndBy` p_newLine
