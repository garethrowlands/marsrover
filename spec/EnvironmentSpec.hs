module EnvironmentSpec where

import Test.QuickCheck
import Test.Hspec
import Data.Set as S
import Data.Maybe (isJust)
import Environment
import Geometry
import Control.Error (isRight, isLeft)
import ArbitraryData ()

spec :: Spec
spec = do

    describe "mkPlateau" $ do
        it "returns a Plateau when right and top are bigger than or equal to zero" $
            property $ \right top -> right >= 0 && top >= 0 ==> isRight (mkPlateau right top)
        it "returns PlateauBoundsOutOfRange when right or top are less then zero" $
            property $ \right top -> right < 0 && top < 0 ==> isLeft (mkPlateau right top)

    describe "outside" $ do
        it "-1,-1 is outside" $
            plateauProperty (\_ _ p -> (-1,-1) `outside` p)
        it "0,0 is inside" $
            plateauProperty (\_ _ p -> not $ (0,0) `outside` p)
        it "right,top is inside" $
            plateauProperty (\r t p -> not $ (r,t) `outside` p)
        it "right+1,top is outside" $
            plateauProperty (\r t p -> (r+1,t) `outside` p)
        it "right,top+1 is outside" $
            plateauProperty (\r t p -> (r,t+1) `outside` p)
        it "right+1,top+1 is outside" $
            plateauProperty (\r t p -> (r+1,t+1) `outside` p)
        it "0,-1 is outside" $
            plateauProperty (\_ _ p -> (0,-1) `outside` p)

    describe "addRoverLocation" $
        it "makes the rover's location an obstacle in the environment" $
           property $ \env rover ->
                let env' = env `addRover` Right rover
                    maybeObstacle = env' `obstacleAt` roverLocation rover
                in  isJust maybeObstacle

        -- it "does not change the environment when given an error instead of a rover" $ do
        --   property $ \env -> (env `addRover` Left "an error") == env

    describe "obstacleAt" $ do

        it "returns PlateauEdge when the given location is outside the plateau" $
            property $ \env@(Environment plateau _) location ->
                location `outside` plateau ==> obstacleAt env location == Just PlateauEdge

        it "returns AnotherRover when another rover is already at the given location" $
            property $ \env@(Environment _ rovers)->
                and [obstacleAt env location == Just AnotherRover|location <- toList rovers]

--            property $ \env location ->
--                not(location `outside` (envPlateau env)) ==>
--                let env' = env `addRover` (Right $ RoverPos location N)
--                in  obstacleAt env' location == Just AnotherRover

plateauProperty :: (Int -> Int -> Plateau -> Bool) -> Property
plateauProperty predicate = property $ \r t -> case mkPlateau r t of
    Right plateau -> predicate r t plateau
    Left _  -> True -- no plateau
