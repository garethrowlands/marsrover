-- |Renders data into the format required in the specification.
-- This module plays the role of the view layer.
-- Implementation is very simple. More sophisticated implementations might use
-- a pretty printing library or a templating library.
module Render

where

import Geometry
import Environment
import Interpreter
import qualified Data.Set as S


-- |Returns either a rendered 'RoverPos' as required by the specification or
-- an error message explaining what the rover hit.
renderRoverState :: RoverState -> String
renderRoverState (Right r) = renderRoverPos r
renderRoverState (Left (r `CrashedInto` o)) = renderRoverPos r ++
    " but crashed into " ++ renderObstacle o ++ "!"
renderRoverState (Left (r `LandedOn` o)) = renderRoverPos r ++
    " but landed on " ++ renderObstacle o ++ "!"

-- |Renders a 'RoverPos' as required by the specification
renderRoverPos :: RoverPos -> String
renderRoverPos (RoverPos (x,y) h) = show x ++ " " ++ show y ++ " " ++ show h

-- |Renders either "another rover" or "the plateau edge"
renderObstacle :: ObstacleType -> String
renderObstacle AnotherRover = "another rover"
renderObstacle PlateauEdge  = "the plateau edge"

-- |Prints the message that plateau bounds are out of range
renderPlateauErr (PlateauBoundsOutOfRange x y) =
    "The plateau bounds ("++show x++","++ show y++") must be zero or greater."

-- |Prints the overall 'RoverState' list
-- (or an error message if the 'Plateau' bounds are wrong)
render (Left err) = renderPlateauErr err
render (Right (results,env)) = renderedResults ++ renderedEnvironment
    where
        renderedResults = unlines . map renderRoverState $ results
        renderedEnvironment = unlines (asciiArt env)


-- asciiArt :: Environment -> [String]
asciiArt (Environment p ls) =
    [
        [renderCh $ isHit (x,y) |x <- [0 .. plateauRight p]] |
        y <- [0 .. plateauTop p]
    ]
    where
        isHit l = S.member l ls
        renderCh True = 'o'
        renderCh False = '-'

