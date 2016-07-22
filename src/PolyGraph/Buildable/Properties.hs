
module PolyGraph.Buildable.Properties where

import PolyGraph.ReadOnly.Graph
import PolyGraph.Buildable
import PolyGraph.Common
import PolyGraph.Common.PropertySupport

buildGraph :: forall g v e t. (Eq v, BuildableGraphDataSet g v e t) =>
                                                           g -> [Either v e] -> g
buildGraph emptyG verticesOrEdges = foldr (flip (?+)) emptyG verticesOrEdges

checkProperty :: forall g v b e. (Eq v,
                                VertexNames v,
                                PairLike e v,
                                MixedBag b v e,
                                BuildableGraphDataSet g v e []) =>
                                   (([e], [v], [v]) -> g -> Bool) -> g ->  b  -> Bool
checkProperty propCondition emptyG bag =
      let graph = buildGraph emptyG (getMix bag) :: g
          bagInfo = analyze bag :: ([e], [v], [v])
      in propCondition bagInfo graph


--
keepVertices :: forall g v e. (Eq v,
                          PairLike e v,
                          BuildableGraphDataSet g v e []) =>
                                           ([e], [v], [v]) -> g -> Bool
keepVertices (edges, isolatedVs, connectedVs) graph =
        ((vCount toPair graph) == (length isolatedVs + length connectedVs))

--
forgetEdges :: forall g v e. (Eq v,
                          PairLike e v,
                          BuildableGraphDataSet g v e []) =>
                                           ([e], [v], [v]) -> g -> Bool
forgetEdges (es, isolatedVs, connectedVs) graph =
               null $ edges graph


--
-- Conveniently typed verification scripts
--
checkMultiEdgeDataProp :: forall g v. (Eq v,
                             VertexNames v,
                             BuildableGraphDataSet g v (UOPair v) []) =>
                                (([UOPair v], [v], [v]) -> g -> Bool) -> g ->  MultiUOBag v -> Bool
checkMultiEdgeDataProp = checkProperty

checkMultiDiEdgeDataProp :: forall g v. (Eq v,
                             VertexNames v,
                             BuildableGraphDataSet g v (OPair v) []) =>
                                (([OPair v], [v], [v]) -> g -> Bool) -> g ->  MultiOBag v -> Bool
checkMultiDiEdgeDataProp = checkProperty
