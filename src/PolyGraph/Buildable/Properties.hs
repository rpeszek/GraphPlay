
module PolyGraph.Buildable.Properties where

import PolyGraph.ReadOnly.Graph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Common
import PolyGraph.Common.PropertySupport

buildGraph :: forall g v e t. (Eq v, BuildableGraphDataSet g v e t) =>
                                                           g -> [Either v e] -> g
buildGraph emptyG verticesOrEdges = foldr (flip (?+)) emptyG verticesOrEdges


prop_notForgetfulG :: forall g v. (Eq v,
                                   VertexNames v,
                                   BuildableGraphDataSet g v (UOPair v) []) =>
                                                       g ->  MultiUOBag v -> Bool
prop_notForgetfulG emptyG bag =
      let graph = buildGraph emptyG (getMix bag) :: g
          (es, isolatedVs, connVs) = analyze bag :: ([UOPair v], [v], [v])
      in ((e graph == length es) && (length (isolatedVertices graph) == length isolatedVs))

--
prop_esForgetfulKeepsVsG :: forall g v. (Eq v,
                                   VertexNames v,
                                   BuildableGraphDataSet g v (UOPair v) []) =>
                                                       g ->  MultiUOBag v -> Bool
prop_esForgetfulKeepsVsG emptyG bag =
      let graph = buildGraph emptyG (getMix bag) :: g
          (es, isolatedVs, connVs) = analyze bag :: ([UOPair v], [v], [v])
      in ((e graph == 0) && (defaultVertexCount toPair graph) == (length connVs + length isolatedVs))
