-- Work in progress

module Buildable.PropertiesSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import PolyGraph.Common
import PolyGraph.Common.PropertySupport
import PolyGraph.Buildable
import PolyGraph.Buildable.Properties
import PolyGraph.Instances.ListGraphs (Vertices, Edges)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Vertices Type" $ do
    context "as Buildable Graph Instance" $ do
      it "has keep vertices and forget edges" $ property $
        checkMultiEdgeDataProp (keepVertices &&& forgetEdges) (emptyGraph :: Vertices Int (UOPair Int))
    context "as Buildable Di-Graph Instance" $ do
      it "has keep vertices and forget di-edges" $ property $
        checkMultiDiEdgeDataProp (keepVertices &&& forgetEdges) (emptyGraph :: Vertices Int (OPair Int))
