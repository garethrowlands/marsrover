-- |The "Geometry" module represents the geometry of the game world.
-- A real Mars rover would likely use real 3D geometry and trigonometry
-- but the game world 'Plateau' is more like a chess board. The implementation
-- here models the game world rather than 3D space.
module Geometry

where

-- |A 'RoverPos' is position of a rover: its location and heading
data RoverPos = RoverPos {
        roverLocation :: Location,
        roverHeading  :: Heading
    } deriving (Show,Eq,Ord)

-- |A 'Location' is an x,y position. Just a type synonym for a pair of 'Int's
type Location = (Int,Int)

-- |A 'MovementVector' is an offset to a 'Location'.
-- Used to represent the distance travelled in a single step.
type MovementVector = Location

-- |A 'Heading' represents the compass direction the rover is facing
data Heading = N -- ^North
             | E -- ^East
             | S -- ^South
             | W -- ^West
    deriving (Show,Eq,Ord,Bounded,Enum)

-- |A 'Rotation' represents a rotation direction.
-- (Note that 'Left' and 'Right' mean something completely different in Haskell.)
data Rotation = AntiClockwise -- ^rotation right
              | Clockwise     -- ^rotation left
    deriving (Show,Eq,Ord,Bounded,Enum)

-- |Given a 'Heading' and a 'Rotation', returns the new 'Heading'
rotate :: Rotation -> Heading -> Heading
rotate AntiClockwise N = W
rotate AntiClockwise W = S
rotate AntiClockwise S = E
rotate AntiClockwise E = N
rotate Clockwise     N = E
rotate Clockwise     W = N
rotate Clockwise     S = W
rotate Clockwise     E = S

-- |Returns the x and y movement components for the given 'Heading'.
-- East is positive x and North is positive y.
vectorFromDirection :: Heading -> MovementVector
vectorFromDirection N = ( 0, 1)
vectorFromDirection W = (-1, 0)
vectorFromDirection S = ( 0,-1)
vectorFromDirection E = ( 1, 0)

-- |Add a 'MovementVector' to a 'Location', returning the new 'Location'.
moveLocation :: MovementVector -> Location -> Location
moveLocation (dx,dy) (x,y) = (x+dx,y+dy)

-- |Move a 'Location' in the given 'Heading'.
-- Implementation is just the composition of two functions:
-- 'vectorFromDirection' then 'moveLocation'.
moveInDirection :: Heading -> Location -> Location
moveInDirection = moveLocation . vectorFromDirection

mkRover :: Location -> Heading -> RoverPos
mkRover loc dir = RoverPos loc dir
