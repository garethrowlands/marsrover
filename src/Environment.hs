-- |The 'Environment' constrains the movement of the rover.
-- It consists of the 'Plateau' and the other rovers.
module Environment
    (
        Plateau(),PlateauOrError,PlateauError(..),mkPlateau,plateauRight,plateauTop,outside,
        Environment(..),mkEmptyEnvironment,addRover,
        ObstacleType(..),obstacleAt
    )
where

-- import Control.Monad.Error
import Data.Set as S
import Geometry

-- |Represents the plateau dimensions.
-- Because 'plateauRight' and 'plateauTop' must be 0 or greater,
-- 'mkPlateau' must be used to construct it and the raw value constructor 'Plateau'
-- is not exported - it is an abstract type.
data Plateau = Plateau {
    plateauRight  :: Int,
    plateauTop :: Int
}   deriving (Show,Eq,Ord)

-- |Constructs a 'Plateau' with the given top right, assuming bottom left is (0,0)
-- Returns 'Left' 'PlateauError' if there's a problem with the input.
mkPlateau :: Int -> Int -> PlateauOrError
mkPlateau right top
    | right<0 || top<0 = Left $ PlateauBoundsOutOfRange right top
    | otherwise        = Right $ Plateau right top

-- |Represents an error with 'Plateau' input.
data PlateauError = PlateauBoundsOutOfRange Int Int
    deriving (Show,Eq,Ord)

-- |The result of constructing a 'Plateau': 'Either' the 'Plateau' or a 'PlateauError'
type PlateauOrError = Either PlateauError Plateau

-- |Makes 'PlateauError' an instance of 'Error' so it can be used with the 'Either' monad
--  from "Control.Monad.Error".
-- instance Error PlateauError where
--    strMsg = error

-- |Enumerates the kinds of thing a rover can collide with.
data ObstacleType = AnotherRover
                  | PlateauEdge
    deriving (Show,Eq,Ord)

-- |Represents the rover's environment:
-- the plateau and the other rovers it can crash into.
data Environment = Environment {
    envPlateau :: Plateau,
    envRovers  :: Set Location
}   deriving (Show,Eq,Ord)

-- |Determines whether the given 'Location' is free to move to or
-- blocked by the 'Plateau' edge or another rover.
obstacleAt :: Environment -> Location -> Maybe ObstacleType
obstacleAt (Environment plateau rovers) l
    | l `outside` plateau = Just PlateauEdge
    | l `member` rovers   = Just AnotherRover
    | otherwise           = Nothing

-- |True if the given 'Location' is outside the 'Plateau' and False otherwise
outside :: Location -> Plateau -> Bool
outside (x,y) (Plateau r t) = x<0 || x>r || y<0 || y>t

-- |Given a 'Plateau', creates an 'Environment' with no rovers on it.
mkEmptyEnvironment :: Plateau -> Environment
mkEmptyEnvironment p = Environment {
        envPlateau = p,
        envRovers  = S.empty
    }

-- |Add a rover to the 'Environment'. Broken rovers leave it unchanged.
addRover :: Environment -> Either t RoverPos -> Environment
env `addRover` (Right (RoverPos l _)) = env {
        envRovers = l `S.insert` envRovers env
    }
env `addRover` (Left _) = env
