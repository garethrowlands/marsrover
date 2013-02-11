{-# LINE 1 "C:\\Users\\gareth.rowlands\\Documents\\GitHub\\marsrover\\spec\\Spec.hs" #-}module Main where
import Test.Hspec
import qualified EnvironmentSpec
import qualified GeometrySpec
main :: IO ()
main = hspec $ describe "Environment" EnvironmentSpec.spec >> describe "Geometry" GeometrySpec.spec
