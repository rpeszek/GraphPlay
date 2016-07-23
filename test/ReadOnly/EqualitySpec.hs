
module ReadOnly.EqualitySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import PolyGraph.Common
import PolyGraph.Buildable
import PolyGraph.Buildable.Properties
import PolyGraph.Common.PropertySupport
import qualified PolyGraph.ReadOnly.Graph.Properties as GProps
import qualified PolyGraph.ReadOnly.DiGraph.Properties as DiGProps

import PolyGraph.ReadOnly.DiGraph.DiGraphEquality ((~>#==))
import PolyGraph.ReadOnly.Graph.GraphEquality

import qualified PolyGraph.Instances.ListGraphs as List
import qualified PolyGraph.Instances.EdgeCountMapGraph as Adj
import qualified PolyGraph.Instances.DiGraph.DiEdgesByVertexMap as Indx

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "edgeCount equality (~>#==)" $ do
    it "is reflexive" $ property $
            (\(mixedEsAndVsBag :: (MultiOBag Int)) ->
                  let graph :: Indx.DiEdgeListByVertexMap Int (OPair Int)
                      graph = buildGraph emptyGraph (getMix mixedEsAndVsBag)
                  in (graph ~>#== graph)
            )
{-

    it "two graph types with the same forgetfulness created the same way are equal" $ property $
            (\(mixedEsAndVsBag :: (MultiOBag Int)) ->
                  let graph1 :: Indx.DiEdgeListByVertexMap Int (OPair Int)
                      graph1 = buildGraph emptyGraph (getMix mixedEsAndVsBag)
                      graph2 :: Adj.EdgeCountMapGraph Int
                      graph2 = buildGraph emptyGraph (getMix mixedEsAndVsBag)
                  in (graph1 ~>#== graph2)
            )
-}
