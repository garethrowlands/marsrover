{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances,
    ScopedTypeVariables
 #-}
module ArbitraryData where

import Test.QuickCheck
import Control.Applicative
import Geometry
import Environment
import Data.Set as S
import Control.Error (isRight)

-- instance Arbitrary (S.Set Location) where
--    arbitrary = S.fromList <$> arbitrary
    
instance Arbitrary RoverPos where
    arbitrary = RoverPos <$> arbitrary <*> arbitrary

instance (Enum a, Bounded a) => Arbitrary a where
    arbitrary = elements [minBound .. maxBound]

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

    