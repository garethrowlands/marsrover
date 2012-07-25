
import Test.DocTest
main = doctest ["--optghc=-isrc", "-v", "src/Main.hs"]