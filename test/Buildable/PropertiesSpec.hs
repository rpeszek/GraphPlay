-- Work in progress

module Buildable.PropertiesSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import PolyGraph.Common
import PolyGraph.Common.PropertySupport ((&&&))
import PolyGraph.Buildable
import PolyGraph.Buildable.Properties
import PolyGraph.Instances.ListGraphs (Vertices, Edges)
import PolyGraph.Instances.SimpleGraph

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Vertices Type" $ do
    context "as Buildable Graph Instance" $ do
      it "has keep vertices and forget edges" $ property $
        checkMultiEdgeDataProp (keepVertices &&& forgetEdges) (on :: Vertices Int (UOPair Int))
    context "as Buildable Di-Graph Instance" $ do
      it "has keep vertices and forget di-edges" $ property $
        checkMultiDiEdgeDataProp (keepVertices &&& forgetEdges) (on :: Vertices Int (OPair Int))

  describe "Edges Type" $ do
    context "as Buildable Graph Instance" $ do
      it "has keep all (including multi) edges and forget isolated vertices" $ property $
         checkMultiEdgeDataProp (keepAllEdges &&& forgetIsolatedVertices) (on :: Edges Int (UOPair Int))
    context "as Buildable Di-Graph Instance" $ do
      it "has keep all (including multi) edges and forget isolated vertices" $ property $
         checkMultiDiEdgeDataProp (keepAllEdges &&& forgetIsolatedVertices) (on :: Edges Int (OPair Int))

  describe "SimpleGraph Type" $ do
    context "as Buildable Graph Instance" $ do
      it "has forget multi edges and keep vertices" $ property $
         checkMultiEdgeDataProp (forgetMultiEdges &&& keepVertices) (on :: SimpleListGraph Int )
