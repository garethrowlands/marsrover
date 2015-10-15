module InterpreterSpec where

import Controller
import Environment
import Geometry
import Commands
import Test.Hspec
import ArbitraryData
import Control.Error
import Data.Set (singleton, empty)
import Test.QuickCheck
-- import Test.QuickCheck.Modifiers (Positive)

doCmd :: Environment -> RoverPos -> Command -> Either RoverError RoverPos
-- doCmd _ (RoverPos (x,y) h) Forwards = Right $ RoverPos (moveInDirection h (x,y)) h
-- doCmd _ (RoverPos (x,y) h) (Turn r) = Right $ RoverPos (x,y) (rotate r h)

runCmd = undefined

doCmd env (RoverPos (x,y) h) Forwards =
    let r' = RoverPos (moveInDirection h (x,y)) h
    in checkRoverPos CrashedInto env r'
doCmd _ (RoverPos (x,y) h) (Turn r) = Right $ RoverPos (x,y) (rotate r h)

spec :: Spec
spec = do
    describe "Turn command" $ do
        it ("rotates the rover's heading in the given direction," ++
            " leaving the location unchanged") $ do
            property $ \(x,y) (Enum' h) r ->
                let original = RoverPos (x,y) h
                    h'       = rotate r h
                    expected = Right $ RoverPos (x,y) h'
                    newState = doCmd undefined original (Turn r)
                in newState == expected

    describe "Forwards command" $ do

        it "moves the rover's location one unit in the direction it is heading" $ do
            property $ \(Positive x, Positive y) (Enum' h) ->
                                   let r = RoverPos (x,y) h
                                       Right plateau = mkPlateau (x+1) (y+1)
                                       env = mkEmptyEnvironment plateau
                                       r' = doCmd env r Forwards
                                   in r' == Right (RoverPos (moveInDirection h (x,y)) h)

        it "always succeeds in an empty environment away from the plateau edge" $ do
            property $ \(Positive x, Positive y) (Enum' h) ->
                let Right plateau = mkPlateau (x+1) (y+1)
                    r = RoverPos (x,y) h
                    environment = Environment plateau empty
                in isRight $ doCmd environment r Forwards

        it "returns an error when there is an obstacle in the environment at that location" $ do
            property $ \(Positive x, Positive y) (Enum' h) ->
                let Right plateau = mkPlateau (x+1) (y+1)
                    r = RoverPos (x,y) h
                    newLocation = moveInDirection h (x,y)
                    environment = Environment plateau (singleton newLocation)
                in isLeft $ doCmd environment r Forwards

    describe "checkRoverPos" $ do

        it "returns Right rover when its location is empty" $ do
            pending

        -- it "returns Left RoverError when there is an obstacle at its location" $ do
        --     property $ \env r@(RoverPos l h) ->
        --         let expectedError = r `CrashedInto` PlateauEdge
        --
        --         in r
