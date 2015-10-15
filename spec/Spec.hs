-- Can generate this file with {# OPTIONS_GHC -F -pgmF hspec-discover #}

module Main where
import qualified AcceptanceSpec
import qualified EnvironmentSpec
import qualified GeometrySpec
import qualified InterpreterSpec
import qualified ParserSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Acceptance" AcceptanceSpec.spec
  describe "Environment" EnvironmentSpec.spec
  describe "Geometry" GeometrySpec.spec
  describe "Interpreter" InterpreterSpec.spec
  describe "Parser" ParserSpec.spec
