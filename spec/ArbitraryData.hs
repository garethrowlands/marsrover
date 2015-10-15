{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, GeneralizedNewtypeDeriving
 #-}

module ArbitraryData where

import Test.QuickCheck
import Geometry
import Environment
import Data.Set as S

-- instance Arbitrary (S.Set Location) where
--    arbitrary = S.fromList <$> arbitrary

newtype Enum' a = Enum' { enum :: a }
  deriving (Bounded, Enum, Show)

instance Arbitrary RoverPos where
    arbitrary = RoverPos <$> arbitrary <*> (enum <$> arbitrary)

instance (Enum a, Bounded a) => Arbitrary (Enum' a) where
     arbitrary = elements [minBound.. maxBound]

instance Arbitrary Rotation where
     arbitrary = enum <$> arbitrary

instance Arbitrary Environment where
    arbitrary = do
        NonNegative r <- arbitrary
        NonNegative t <- arbitrary
        let Right plateau = mkPlateau r t
        locations <- setOf (arbitraryPlateauLocation r t)
        return $ Environment plateau locations
            where arbitraryPlateauLocation r t = (,) <$> choose (0,r) <*> choose (0,t)
                  setOf a = S.fromList <$> listOf a


instance Arbitrary PlateauOrError where
    arbitrary = mkPlateau <$> arbitrary <*> arbitrary
