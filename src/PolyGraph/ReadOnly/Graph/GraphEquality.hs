{-
  Graphs are considered equal (using relaxed (~#==) notation if they have the same vertices
  and the same vertex adjacency (each 2 verices have exactly the same number of edges connecting them)

  TODO this needs testing
-}
module PolyGraph.ReadOnly.Graph.GraphEquality where

import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Adjustable.GDSAdjust
import PolyGraph.Common.Helpers
import PolyGraph.Buildable.PolyMorth
import Data.Hashable
import PolyGraph.Instances.EdgeCountMapGraph

--morth :: forall g0 v0 e0 t0 g1 v1 e1 t1. (GraphDataSet g0 v0 e0 t0, BuildableGraphDataSet g1 v1 e1 t1) =>
--                GMorphism v0 e0 v1 e1 -> g0 -> g1

edgeCountGIsomorphism :: (EdgeSemantics e v) => GMorphism v e v (UOPair v)
edgeCountGIsomorphism = GMorphism {
     vTrans = id,
     eTrans = resolveEdge
  }

(~#==) :: forall g v e t . (Eq v, Hashable v, Graph g v e t) => g -> g -> Bool
g0 ~#== g1 =
             let g0edgeCounts :: EdgeCountMapGraph v
                 g0edgeCounts = morth edgeCountGIsomorphism g0
                 g1edgeCounts :: EdgeCountMapGraph v
                 g1edgeCounts = morth edgeCountGIsomorphism g1
              in g0edgeCounts == g1edgeCounts