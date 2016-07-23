
module PolyGraph.Buildable.Properties where

import PolyGraph.ReadOnly
import PolyGraph.ReadOnly.Graph
import PolyGraph.Buildable
import PolyGraph.Common
import PolyGraph.Common.PropertySupport
import Data.List (nub)

--
keepVertices :: forall g v e t. (Eq v,
                          PairLike e v,
                          BuildableGraphDataSet g v e t) =>
                                           ([e], [v], [v]) -> g -> Bool
keepVertices (edges, isolatedVs, connectedVs) graph =
        ((vCount toPair graph) == (length isolatedVs + length connectedVs))

--
forgetIsolatedVertices :: forall g v e t. (Eq v,
                          PairLike e v,
                          BuildableGraphDataSet g v e t) =>
                                           ([e], [v], [v]) -> g -> Bool
forgetIsolatedVertices (edges, isolatedVs, connectedVs) graph =
         ((vCount toPair graph) == (length connectedVs))

--
forgetEdges :: forall g v e t. (Eq v,
                          PairLike e v,
                          BuildableGraphDataSet g v e t) =>
                                           ([e], [v], [v]) -> g -> Bool
forgetEdges (es, isolatedVs, connectedVs) graph =
               null $ edges graph

--
keepAllEdges :: forall g v e t. (Eq v,
                          PairLike e v,
                          BuildableGraphDataSet g v e t) =>
                                           ([e], [v], [v]) -> g -> Bool
keepAllEdges (es, isolatedVs, connectedVs) graph =
                  eCount graph == length es

--
forgetMultiEdges :: forall g v e t. (Eq v,
                          Eq e,
                          PairLike e v,
                          BuildableGraphDataSet g v e t) =>
                                           ([e], [v], [v]) -> g -> Bool
forgetMultiEdges (es, isolatedVs, connectedVs) graph =
                   eCount graph == (length . nub $ es)

--
--  Logic to check above properties by building graph
--
buildGraph :: forall g v e t. (Eq v, BuildableGraphDataSet g v e t) =>
                                                           g -> [Either v e] -> g
buildGraph emptyG verticesOrEdges = foldr (flip (?+)) emptyG verticesOrEdges

checkProperty :: forall g v b e t. (Eq v,
                                VertexNames v,
                                PairLike e v,
                                MixedBag b v e,
                                BuildableGraphDataSet g v e t) =>
                                   (([e], [v], [v]) -> g -> Bool) -> g ->  b  -> Bool
checkProperty propCondition emptyG bag =
      let graph = buildGraph emptyG (getMix bag) :: g
          bagInfo = analyze bag :: ([e], [v], [v])
      in propCondition bagInfo graph

on :: forall g v e t. BuildableGraphDataSet g v e t => g
on = emptyGraph

--
-- Conveniently typed verification scripts
--
checkMultiEdgeDataProp :: forall g v t. (Eq v,
                             VertexNames v,
                             BuildableGraphDataSet g v (UOPair v) t) =>
                                (([UOPair v], [v], [v]) -> g -> Bool) -> g ->  MultiUOBag v -> Bool
checkMultiEdgeDataProp = checkProperty

checkMultiDiEdgeDataProp :: forall g v t. (Eq v,
                             VertexNames v,
                             BuildableGraphDataSet g v (OPair v) t) =>
                                (([OPair v], [v], [v]) -> g -> Bool) -> g ->  MultiOBag v -> Bool
checkMultiDiEdgeDataProp = checkProperty

checkSimpleEdgeDataProp :: forall g v t. (Eq v,
                             VertexNames v,
                             BuildableGraphDataSet g v (UOPair v) t) =>
                                (([UOPair v], [v], [v]) -> g -> Bool) -> g ->  SimpleUOBag v -> Bool
checkSimpleEdgeDataProp = checkProperty

checkSimpleDiEdgeDataProp :: forall g v t. (Eq v,
                             VertexNames v,
                             BuildableGraphDataSet g v (OPair v) t) =>
                                (([OPair v], [v], [v]) -> g -> Bool) -> g ->  SimpleOBag v -> Bool
checkSimpleDiEdgeDataProp = checkProperty
