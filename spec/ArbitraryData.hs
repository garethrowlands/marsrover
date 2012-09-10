{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances,
    ScopedTypeVariables
 #-}
module ArbitraryData where

import Test.QuickCheck
import Control.Applicative
import Geometry
import Environment
import Data.Set as S

instance Arbitrary (S.Set Location) where
    arbitrary = S.fromList <$> arbitrary
    
instance Arbitrary RoverPos where
    arbitrary = RoverPos <$> arbitrary <*> arbitrary

instance (Enum a, Bounded a) => Arbitrary a where
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary Environment where
    arbitrary = do
        Right plateau <- (mkPlateau <$> arbitrary <*> arbitrary) `suchThat` isRight
        let r = plateauRight plateau
            t = plateauTop   plateau
        locations <- S.fromList <$> listOf (arbitraryTupleUpTo r t)
        return $ Environment plateau locations
            where arbitraryTupleUpTo r t = do
                    width <- choose (0, r)
                    height <- choose (0, t)
                    return (width,height)

isRight (Left _) = False
isRight (Right _) = True

instance Arbitrary PlateauOrError where
    arbitrary = mkPlateau <$> arbitrary <*> arbitrary

    