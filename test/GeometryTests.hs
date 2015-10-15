-- |Tests the "Navigation" module
module GeometryTests

where

import Test.HUnit
import Geometry

-- |Build a test that checks that applying a given function to a given value
-- leaves that value unchanged
-- assertIdentity :: (a -> a) -> a -> Test
assertIdentity f x = f x ~=? x

compassDirs = [N,E,S,W] -- enumFrom minBound

rotate4IsIdentityTest = "rotate 4 times from x == x" ~: test [
        show "rotate "++show rotationDir++" starting "++show compassDir ~:
        iterate (rotate rotationDir) compassDir !! 4 ~=? compassDir |
        rotationDir <- [Clockwise, AntiClockwise],
        compassDir <- compassDirs
    ]

directionsAreInversesTest = "rotate one way then the other way leaves direction unchanged" ~: test [
        "rotate "++show d1++" then "++show d2 ~:
        assertIdentity (rotate d2 . rotate d1) compass |
        (d1,d2) <- [(Clockwise,AntiClockwise),(AntiClockwise,Clockwise)],
        compass <- compassDirs
    ]

moveInDirectionAddsMovementVectorTest =
    "moving a RoverPos forwards changes the Location by " ++
    "the MovementVector for its Heading" ~:
    test [
        "starting from "++show x++","++show y++" "++ show h ~:
        moveInDirection h (x,y) ~=? (x+dx,y+dy) |
        x <- [1,10..100], y <- [1,2..10], h <- compassDirs,
        let (dx,dy) = vectorFromDirection h
    ]

allNavigationTests = "navigation" ~: test [
        rotate4IsIdentityTest,
        directionsAreInversesTest,
        moveInDirectionAddsMovementVectorTest
    ]
