-- Work in progress

module Buildable.PropertiesSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import PolyGraph.Common
import PolyGraph.Common.PropertySupport ((&&&), analyze, MultiOBag, MixedBag(..))
import PolyGraph.Buildable
import PolyGraph.Buildable.Properties
import qualified PolyGraph.Instances.ListGraphs as List
import qualified PolyGraph.Instances.SimpleGraph as Simple
import qualified PolyGraph.Instances.EdgeCountMapGraph as Adj
import qualified PolyGraph.Instances.DiGraph.DiEdgesByVertexMap as Indx

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Vertices Type" $ do
    context "as Buildable Graph instance" $ do
      it "has keep-vertices and forget-edges" $ property $
        checkMultiEdgeDataProp (keepVertices &&& forgetEdges) (on :: List.Vertices Int (UOPair Int))
    context "as Buildable Di-Graph instance" $ do
      it "has keep-vertices and forget-di-edges" $ property $
        checkMultiDiEdgeDataProp (keepVertices &&& forgetEdges) (on :: List.Vertices Int (OPair Int))

    context "in spelled out code" $ do
      it "Di-Graph instance has keep-vertices and forget-di-edges" $ property $
          (\(mixedEsAndVsBag :: (MultiOBag Int)) ->
                let graph :: List.Vertices Int (OPair Int)
                    graph = buildGraph emptyGraph (getMix mixedEsAndVsBag)
                    bagElements = analyze mixedEsAndVsBag
                in (keepVertices &&& forgetEdges) bagElements graph
          )

  describe "Edges Type" $ do
    context "as Buildable Graph Instance" $ do
      it "has keep-all-edges (including multi) and forget-isolated-vertices" $ property $
         checkMultiEdgeDataProp (keepAllEdges &&& forgetIsolatedVertices) (on :: List.Edges Int (UOPair Int))
    context "ditto as Di-Graph" $ do
      it "has keep-all-edges (including multi) and forget-isolated-vertices" $ property $
         checkMultiDiEdgeDataProp (keepAllEdges &&& forgetIsolatedVertices) (on :: List.Edges Int (OPair Int))

  describe "SimpleGraph Type" $ do
    context "as Buildable Graph Instance" $ do
      it "has (List) forget-multi-edges and keep-vertices" $ property $
         checkMultiEdgeDataProp (forgetMultiEdges &&& keepVertices) (on :: Simple.SimpleListGraph Int )
      it "ditto for Simple(Set)Graph" $ property $
         checkMultiEdgeDataProp (forgetMultiEdges &&& keepVertices) (on :: Simple.SimpleSetGraph Int )
    context "as Buildable Di-Graph" $ do
      it "has (List) forget-multi-edges and keep-vertices" $ property $
         checkMultiDiEdgeDataProp (forgetMultiEdges &&& keepVertices) (on :: Simple.SimpleListDiGraph Int )
      it "ditto for Simple(Set)DiGraph" $ property $
         checkMultiDiEdgeDataProp (forgetMultiEdges &&& keepVertices) (on :: Simple.SimpleSetDiGraph Int )

  describe "Adjacency Matrix (EdgeCount Map Graph)" $ do
    context "as Buildable Graph Instance" $ do
      it "has keep-all-edges and keep-vertices" $ property $
        checkMultiEdgeDataProp (keepAllEdges &&& keepVertices) (on :: Adj.EdgeCountMapGraph Int)
    context "ditto as Di-Graph" $ do
      it "has keep-all-edges and keep-vertices" $ property $
        checkMultiDiEdgeDataProp (keepAllEdges &&& keepVertices) (on :: Adj.EdgeCountMapDiGraph Int)

  describe "DiEdgesByVertexMap Di-Graph" $ do
    it "has keep-all-edges and keep-vertices" $ property $
        checkMultiDiEdgeDataProp (keepAllEdges &&& keepVertices) (on :: Indx.DiEdgeListByVertexMap Int (OPair Int))
