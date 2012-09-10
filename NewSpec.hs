{-# LINE 1 ".\\spec\\Spec.hs" #-}module Main where
import Test.Hspec
import qualified GeometrySpec
main :: IO ()
main = hspec $ describe "Geometry" GeometrySpec.spec
