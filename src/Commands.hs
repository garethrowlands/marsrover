-- |Declares data to represent the input command stream
module Commands

where

import Geometry
import Environment

-- |Represents the overall input stream, which looks like this:
--
-- @
--   5 5
--   1 2 N
--   LMLMLMLMM
--   3 3 E
--   MMRMMRMRRM
-- @
data OverallInput = OverallInput {
    inputPlateau :: PlateauOrError,
    inputRovers  :: [RoverInput]
} deriving (Show,Eq,Ord)


-- |Represents the input stream for a particular 'Rover', which looks like this:
--
-- @
--   1 2 N
--   LMLMLMLMM
-- @
data RoverInput = RoverInput {
    inputRover   :: RoverPos,
    inputCommands :: [Command]
} deriving (Show,Eq,Ord)

-- |Represents a single command: either a turn left/right or move forwards.
--  Looks like one of these:
--
-- @
--   L
--   R
--   M
-- @
data Command = Turn Rotation  -- ^rotate the rover
             | Forwards       -- ^move the rover forwards
    deriving (Show,Eq,Ord)
