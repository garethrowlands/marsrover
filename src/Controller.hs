-- |"Controller" is an interpreter for the input commands.
module Controller

where

import Control.Monad.State
import Control.Monad.Error
import Data.Set as S

import Environment
import Geometry
import Commands
import Parser

-- |Represents an error that occurred during rover execution:
-- a bad start position or crashing into something.
-- (The backticks around the value constructor 'CrashedInto'
-- let it appear between its arguments, which reads more
-- naturally than if it appeared first.)
data RoverError = RoverPos `LandedOn` ObstacleType    -- ^Bad start position
                | RoverPos `CrashedInto` ObstacleType -- ^Crashed when moving
                | OtherRoverError String              -- ^To make 'RoverError' an
                                                      --  instance of the 'Error' class
    deriving (Show,Eq,Ord)

-- |Makes 'RoverError' an instance of 'Error' so that the 'Either' monad works.
instance Error RoverError where
    strMsg = OtherRoverError

-- |Running a rover results in either an error or the new 'RoverPos'
type RoverState = Either RoverError RoverPos

-- |Main entry point. Returns the overall answer, given the (parsed) overall input.
--  If the input does not contain a valid 'Plateau' then return 'Left PlateauError'.
--  Otherwise, return a tuple of:
--
--    * the results of running the rovers
--
--    * the final state of the 'Environment'
--
--  Uses the 'State' monad's 'runState' function as follows:
--
--    * Initial state is the 'Environment' corresponding to the given 'Plateau' being empty.
--
--    * The function run with this state as context maps the each rover input to
--      its corresponding final rover state, updating the 'Environment' as it goes.
runOverallInput :: OverallInput -> Either PlateauError ([RoverState],Environment)
runOverallInput (OverallInput plateauOrError roverInputs) = do
    plateau <- plateauOrError
    let env = mkEmptyEnvironment plateau
    return $ runState (mapM runRoverInput roverInputs) env

-- |Runs a rover with the given 'Environment' state;
--  updates the state by adding the rover's end position to it.
runRoverInput :: RoverInput -> State Environment RoverState
runRoverInput (RoverInput r cmds) = do
    env <- get
    let r' = runRover env cmds r
    put $ env `addRover` r'
    return r'

-- |Introduces a rover to the 'Environment'; the resulting 'RoverState' contains 'Either'
--  the supplied 'RoverPos' or a 'RoverError' if there is an obstacle at that 'Location'.
landRover :: Environment -> RoverPos -> RoverState
landRover env r = checkRoverPos LandedOn env r

-- |The type of functions that build 'RoverError' values
type RoverErrorBuilder = RoverPos -> ObstacleType -> RoverError

-- |Check the given location against the 'Environment'.
-- If there's an obstacle there, build a 'RoverError' using the supplied function.
-- This allows us to use the same function on the initial landing and when moving.
checkRoverPos :: RoverErrorBuilder -> Environment -> RoverPos -> RoverState
checkRoverPos errorFunc env r@(RoverPos l _) =
    case env `obstacleAt` l of
        Nothing -> return r
        Just obstacle -> throwError $ r `errorFunc` obstacle

-- |Runs a single rover within the given 'Environment'.
-- Runs in the error monad, which terminates immediately when a computation returns a 'Left'.
-- Both 'landRover' and 'execute' return 'Left' to indicate a crashed rover.
-- Having landed the rover, it folds the list of commands into a single result using
runRover :: Environment -> [Command] -> RoverPos -> RoverState
runRover env cmds r = do
    landRover env r
    foldM (runCommand env) r cmds

-- |Execute the given 'Commmand', returning the new 'RoverState':
--  'Either' a new 'RoverPos' or a 'RoverError'
runCommand :: Environment -> RoverPos -> Command -> RoverState
runCommand env (RoverPos l h) Forwards = do
    let l' = moveInDirection h l
        r' = RoverPos l' h
    checkRoverPos CrashedInto env r'
runCommand env (RoverPos l h) (Turn dir) = do
    let h' = rotate dir h
    return $ RoverPos l h'

