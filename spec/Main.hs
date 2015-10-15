import Test.Hspec

import qualified AcceptanceSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Sample"     FooSpec.spec
