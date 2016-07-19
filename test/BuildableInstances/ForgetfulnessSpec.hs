-- Work in progress

module BuildableInstances.ForgetfulnessSpec (main, spec) where

import Test.Hspec
import qualified Play.DiGraph.SampleData as SD

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sanity" $ do
    it "tests should run" $ do
      length [1..10] == 10
    it "and should access other code" $ do
      length SD.testEdges == 10
