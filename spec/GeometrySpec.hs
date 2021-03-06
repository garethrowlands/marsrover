{-# LANGUAGE ScopedTypeVariables
 #-}

module GeometrySpec where

import Test.QuickCheck
import Test.Hspec.QuickCheck ()
import Test.Hspec
import Geometry
import ArbitraryData (Enum'(..))


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "rotate" $ do
        -- it "returns to the same heading when applied 4 times in the same direction" $
        --     -- property $ \heading rotation -> (rotate rotation . rotate rotation . rotate rotation . rotate rotation) heading == heading
        --     -- property $ \heading rotation -> iterate (rotate rotation) heading !! 4 == heading
        --     test [(rotate rotation . rotate rotation . rotate rotation . rotate rotation) heading `shouldBe` heading |
        --         rotation <- [Clockwise, AntiClockwise],
        --         heading <- [N,E,S,W]]
        --     -- (rotate AntiClockwise . rotate AntiClockwise . rotate AntiClockwise . rotate AntiClockwise) N ~=? N

        it "returns to the same heading when applied once in each direction" $
            property $ \(Enum' heading) ->
              ((rotate Clockwise . rotate AntiClockwise) heading == heading) &&
              ((rotate AntiClockwise . rotate Clockwise) heading == heading)

        it "returns W when rotated AntiClockwise from N" $
            rotate AntiClockwise N `shouldBe` W

    describe "moveInDirection" $ do
        it "returns (x, y+1) when given N and (x, y): 'the square directly North from (x, y) is (x, y+1)'" $
            property $ \(x,y) -> moveInDirection N (x,y) == (x, y+1)

        it "returns to the same location when given one step in each direction" $
            let moveInDirection' = flip moveInDirection
            in
              property $ \location -> foldl moveInDirection' location [N,E,S,W] == location
