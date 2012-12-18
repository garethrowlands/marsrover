-- |Test the view layer - that the output data is converted to strings as specified
module Test.RenderTests

where

import Geometry
import Environment
import Render
import Controller
import Test.HUnit

aRover = RoverPos (2,3) N

-- |Tests rendered 'RoverResult' meets specification
testRenderResult :: Test
testRenderResult = "RoverResult" ~: test [
        "crash: plateau edge" ~:
        renderRoverState (Left (aRover `CrashedInto` PlateauEdge)) ~=?
        "2 3 N but crashed into the plateau edge!",

        "crash: another rover" ~:
        renderRoverState (Left (aRover `CrashedInto` AnotherRover)) ~=?
        "2 3 N but crashed into another rover!",

        "success" ~:
        renderRoverState (Right aRover) ~=?
        "2 3 N"
    ]

-- |Tests rendered 'RoverPos' meets specification: X Y N/S/E/W
testRenderRoverPos :: Test
testRenderRoverPos = "RoverPos" ~: renderRoverPos aRover ~=? "2 3 N"


-- |Tests rendered 'ObstacleType'
testRenderObstacle :: Test
testRenderObstacle = "ObstacleType" ~: test [
        renderObstacle AnotherRover ~=? "another rover",
        renderObstacle PlateauEdge  ~=? "the plateau edge"
    ]

-- |Tests rendered 'PlateauError'
testRenderPlateauErr :: Test
testRenderPlateauErr = renderPlateauErr (PlateauBoundsOutOfRange 10 20) ~=?
                "The plateau bounds (10,20) must be zero or greater."

-- |Test suite containing all render tests
allRenderTests :: Test
allRenderTests = "Render" ~: test [
        testRenderResult,
        testRenderRoverPos,
        testRenderObstacle,
        testRenderPlateauErr
    ]

