
module EnvironmentSpec where

import Test.HUnit
import Test.QuickCheck
import Test.Hspec.QuickCheck ()
import Test.Hspec.Monadic
import Data.Set as S
import Data.Maybe (isJust,isNothing)
import Environment
import Geometry
import ArbitraryData (isRight)

spec = do

    describe "mkPlateau" $ do
    
        it "returns a Plateau when right and top are bigger than or equal to zero" $ do
            property $ \right top -> right >= 0 && top >= 0 ==> (isRight $ mkPlateau right top)
             
        it "returns PlateauBoundsOutOfRange when right or top are less then zero" $ do
            property $ \right top -> right < 0 && top < 0 ==> (isLeft $ mkPlateau right top) 
            
            
    describe "outside" $ do
        it "-1,-1 is outside" $ do
            plateauProperty (\r t p -> (-1,-1) `outside` p)
        it "0,0 is inside" $ do
            plateauProperty (\r t p -> not $ (0,0) `outside` p)
        it "right,top is inside" $ do
            plateauProperty (\r t p -> not $ (r,t) `outside` p)
        it "right+1,top is outside" $ do
            plateauProperty (\r t p -> (r+1,t) `outside` p)
        it "right,top+1 is outside" $ do
            plateauProperty (\r t p -> (r,t+1) `outside` p)
        it "right+1,top+1 is outside" $ do
            plateauProperty (\r t p -> (r+1,t+1) `outside` p)
        it "0,-1 is outside" $ do
            plateauProperty (\r t p -> (0,-1) `outside` p)

    describe "addRover" $ do
        it "makes the rover's location an obstacle in the environment" $ do
           property $ \env rover ->
                let env' = env `addRover` Right rover
                    maybeObstacle = env' `obstacleAt` (roverLocation rover) 
                in  (not . isNothing) maybeObstacle

        it "does not change the environment when given an error instead of a rover" $ do
           property $ \env -> (env `addRover` Left "an error") == env
           
    describe "obstacleAt" $ do
    
        it "returns PlateauEdge when the given location is outside the plateau" $ do
            property $ \env@(Environment plateau _) location ->
                location `outside` plateau ==> obstacleAt env location == Just PlateauEdge
                
        it "returns AnotherRover when another rover is already at the given location" $ do
            property $ \env location ->
                not(location `outside` (envPlateau env)) ==>
                let env' = env `addRover` (Right $ RoverPos location N)
                in  obstacleAt env' location == Just AnotherRover 

plateauProperty predicate = property $ \r t -> case mkPlateau r t of
    Right plateau -> predicate r t plateau
    Left _  -> True -- no plateau

    
isLeft = not . isRight

