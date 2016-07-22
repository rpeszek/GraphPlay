-- Work in progress

module BuildableInstances.ForgetfulnessSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Play.DiGraph.SampleData as SD
import PolyGraph.Common
import PolyGraph.Buildable
import PolyGraph.Buildable.Properties
import PolyGraph.Instances.ListGraphs


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sanity" $ do
    it "tests should run" $ do
      length [1..10] == 10
    it "and should access other code" $ do
      length SD.testEdges == 10
  describe "Vertices Graph" $ do
    it "is edge only fogetful" $ property $
      runPropertyMG keepsVertices (emptyGraph :: Vertices Int (UOPair Int))
