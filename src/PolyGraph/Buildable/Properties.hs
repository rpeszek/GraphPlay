
module PolyGraph.Buildable.Properties where

import PolyGraph.ReadOnly.Graph
import PolyGraph.Buildable
import PolyGraph.Common
import PolyGraph.Common.PropertySupport

buildGraph :: forall g v e t. (Eq v, BuildableGraphDataSet g v e t) =>
                                                           g -> [Either v e] -> g
buildGraph emptyG verticesOrEdges = foldr (flip (?+)) emptyG verticesOrEdges

runProperty :: forall g v b e. (Eq v,
                                VertexNames v,
                                PairLike e v,
                                MixedBag b v e,
                                BuildableGraphDataSet g v e []) =>
                                   (([e], [v], [v]) -> g -> Bool) -> g ->  b  -> Bool
runProperty propCondition emptyG bag =
      let graph = buildGraph emptyG (getMix bag) :: g
          bagInfo = analyze bag :: ([e], [v], [v])
      in propCondition bagInfo graph

--
runPropertyMG :: forall g v. (Eq v,
                             VertexNames v,
                             BuildableGraphDataSet g v (UOPair v) []) =>
                                (([UOPair v], [v], [v]) -> g -> Bool) -> g ->  MultiUOBag v -> Bool
runPropertyMG = runProperty

--
keepsVertices :: forall g v e. (Eq v,
                          PairLike e v,
                          BuildableGraphDataSet g v e []) =>
                                           ([e], [v], [v]) -> g -> Bool
keepsVertices (edges, isolatedVs, connectedVs) graph =
        ((vCount toPair graph) == (length isolatedVs + length connectedVs))


-- old --
------
prop_notForgetfulG :: forall g v. (Eq v,
                                   VertexNames v,
                                   BuildableGraphDataSet g v (UOPair v) []) =>
                                                       g ->  MultiUOBag v -> Bool
prop_notForgetfulG emptyG bag =
      let graph = buildGraph emptyG (getMix bag) :: g
          (es, isolatedVs, connVs) = analyze bag :: ([UOPair v], [v], [v])
      in ((eCount graph == length es) && (length (isolatedVertices graph) == length isolatedVs))

--
prop_esForgetfulKeepsVs :: forall g v b e. (Eq v,
                                   VertexNames v,
                                   PairLike e v,
                                   MixedBag b v e,
                                   BuildableGraphDataSet g v e []) =>
                                                       g ->  b  -> Bool
prop_esForgetfulKeepsVs emptyG bag =
      let graph = buildGraph emptyG (getMix bag) :: g
          (es, isolatedVs, connVs) = analyze bag :: ([e], [v], [v])
      in ((eCount graph == 0) && (vCount toPair graph) == (length connVs + length isolatedVs))


prop_esForgetfulKeepsVsG :: forall g v. (Eq v,
                                   VertexNames v,
                                   BuildableGraphDataSet g v (UOPair v) []) =>
                                                       g ->  MultiUOBag v -> Bool
prop_esForgetfulKeepsVsG = prop_esForgetfulKeepsVs
