module PolyGraph.Buildable.PolyRebuild where

import PolyGraph.ReadOnly
import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable

addAllEdges :: forall g0 v e t0 g1 t1. (GraphDataSet g0 v e t0, BuildableGraphDataSet g1 v e t1) =>
                   g0 -> g1 -> g1
addAllEdges gs gd =  foldr(flip (~+)) gd (edges gs)

addAllIsolatedVertices :: forall g0 v e t0 g1 t1. (GraphDataSet g0 v e t0, BuildableGraphDataSet g1 v e t1) =>
                  g0 -> g1 -> g1
addAllIsolatedVertices gs gd = foldr(flip (@+)) gd (isolatedVertices gs)

-- | Note: this is the same as (polyMorth id) if e is a Functor
polyRebuild :: forall g0 v e t0 g1 t1. (GraphDataSet g0 v e t0, BuildableGraphDataSet g1 v e t1) =>
                  g0 -> g1
polyRebuild gs =  (addAllEdges gs) . (addAllIsolatedVertices gs) $ emptyGraph
