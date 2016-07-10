
module PolyGraph.DiGraph.Optimize.MaterializedDiGraph (
   DiGraphHelper(..)
   , buidDiGraph
) where

import PolyGraph.Helpers
import PolyGraph.DiGraph
import PolyGraph.Graph
import PolyGraph.DiGraph.Optimize.HashMapCIndex (BuildableCollection)
import PolyGraph.DiGraph.Optimize.MaterializedDiEdge
import qualified PolyGraph.Graph.PolyBuild as PB

------------------------------------------------------------------------------------
-- helpers for building a graph from thigs that have slow edge resolution        ---
-- this basically allows for creating a d-graph if you have a slow               ---
-- e-> (v,v) function and a slow Foldable with a list of all edges               ---
-- created graph will not contain any loose vertices                             ---
------------------------------------------------------------------------------------
data DiGraphHelper v e t = DiGraphHelper {
   helperEdges    :: t (DiEdgeHelper e v),
   helperVertices :: t v  -- note vertices are stored redundantly from edges, TODO think on this
}

instance forall e v t. (Eq v, Foldable t) => GraphDataSet(DiGraphHelper v e t) v (DiEdgeHelper e v) t where
  edges    = helperEdges
  vertices = helperVertices

instance forall e v t. (Eq v, Foldable t) => DiGraph(DiGraphHelper v e t) v (DiEdgeHelper e v) t

-- NOTE this is not smart enough to know that vertex was already added
-- TODO I could introduce here something like BuildableUniqueCollection, should I?
{-
instance forall e v t. (Eq v, BuildableCollection t) => PB.BuildableGraphDataSet(DiGraphHelper v e t) v (DiEdgeHelper e v) t where
  PB.empty = DiGraphHelper {helperEdges = emptyCollection, helperVertices = emptyCollection}
  --PB.(@+) :: g -> v -> g
  g PB.@+ v  =
              let newVertices =  v `prependElement` (helperVertices g)
              in g {helperVertices = newVertices}

  g PB.(~+) e =
               let newEdges = e `helperEdges` (helperEdges g)
                   newVertices = undefined -- TODO we need something like BuildableUniqueCollection aka HashSet
-}

buidDiGraph :: forall t e v t0. (Foldable t, BuildableCollection t0) =>
                                    (e -> (v,v)) -> t e -> DiGraphHelper v e t0
buidDiGraph _slowresolveDiEdge _slowEdgeCollection =
             let _fastDedges = appendDiEdgeHelpers emptyDiEdgeHelperCollection _slowresolveDiEdge _slowEdgeCollection :: t0 (DiEdgeHelper e v)
                 _collectDiEdgeHelperVertices = collectDiEdgeHelperVertices _fastDedges   ::t0 v
             in DiGraphHelper {helperEdges = _fastDedges, helperVertices = _collectDiEdgeHelperVertices}
