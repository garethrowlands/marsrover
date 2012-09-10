{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances,
    ScopedTypeVariables
 #-}

module GeometrySpec where

import Test.QuickCheck
import Test.Hspec.QuickCheck ()
import Test.Hspec.Monadic
import Geometry
import ArbitraryData ()

main = hspec spec

spec = do
    describe "rotate" $ do
        it "returns to the same heading when applied 4 times in the same direction" $ do
            property $ \heading rotation -> iterate (rotate rotation) heading !! 4 == heading
            
        it "returns to the same heading when applied once in each direction" $ do
            property $ \heading -> (rotate Clockwise . rotate AntiClockwise) heading == heading
            property $ \heading -> (rotate AntiClockwise . rotate Clockwise) heading == heading
            
        it "returns W when rotated AntiClockwise from N" $ do
            rotate AntiClockwise N == W 
            
    describe "moveInDirection" $ do
        it "returns (x, y+1) when given N and (x, y): 'the square directly North from (x, y) is (x, y+1)'" $ do
            property $ \(x,y) -> moveInDirection N (x,y) == (x, y+1)

        it "returns to the same location when given one step in each direction" $ do
            let moveInDirection' = flip moveInDirection
            property $ \location -> foldl moveInDirection' location [N,E,S,W] == location
            
    
